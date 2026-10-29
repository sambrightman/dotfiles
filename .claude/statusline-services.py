#!/usr/bin/env python3
"""The status line's GitHub line, and its Apple line (Xcode Cloud,
TestFlight and the App Store), for the project in the given folder.

    statusline-services.py <folder>             print the line from the cache
    statusline-services.py --refresh <root>     fetch everything, rewrite the cache

Printing never waits on the network. It reads what the last refresh saved
under ~/.cache/claude-statusline/, and when that is old it starts a
refresh in the background, which the next status line shows.

GitHub is shown when the repository's origin is on github.com, read with
the gh CLI's login. Its Actions minutes are read with a token of their own,
so gh's login needn't take the broad "user" scope: a fine-grained token
with no repository access and the one account permission "Plan: read",
kept in the login keychain:
    security add-generic-password -U -s claude-statusline-github -a "$USER" -w
GitHub has no API for a personal account's budget, so the limit is set
here by hand, as "githubBudgetDollars" in the config.

Apple is shown when the repository has an Xcode project whose bundle ID is
an app in App Store Connect, read with the API key named in the config,
~/.claude/statusline-services.json:
    {"appStoreConnect": {"keyID": "…", "issuerID": "…"},
     "githubBudgetDollars": 0, "icons": "auto"}
The key itself is ~/.appstoreconnect/private_keys/AuthKey_<keyID>.p8.
Xcode Cloud's compute hours have no API either; the line adds up how long
this month's runs' actions took, which is what Apple counts, and marks the
figure as an estimate.

"icons" is "nerd" for the GitHub and Apple logos from a Nerd Font, "plain"
for text, or "auto" (the default): the logos when a Nerd Font is installed.
"""

import base64
import datetime as dt
import glob
import hashlib
import json
import os
import re
import subprocess
import sys
import time
import urllib.error
import urllib.request

HOME = os.path.expanduser("~")
CONFIG = os.path.join(HOME, ".claude", "statusline-services.json")
CACHE_DIR = os.path.join(HOME, ".cache", "claude-statusline")

# How old each part may get before a refresh fetches it again, in seconds.
TTL = {"github": 180, "billing": 1800, "apple": 180}
# A refresh that has not finished in this long is taken to have died.
LOCK_TIMEOUT = 300

XCODE_CLOUD_HOURS = 25  # included with the Apple Developer Program, a month
FREE_MINUTES = {"free": 2000, "pro": 3000, "team": 3000}  # private repositories, a month
# Included minutes go further on Linux: Windows uses them twice as fast, macOS ten times.
MINUTE_MULTIPLIER = {"linux": 1, "windows": 2, "macos": 10}

RESET, DIM, BOLD = "\033[0m", "\033[2m", "\033[1m"
RED, GREEN, YELLOW, BLUE, MAGENTA, CYAN = (f"\033[{n}m" for n in range(31, 37))


def load_json(path, default=None):
    try:
        with open(path, encoding="utf-8") as file:
            return json.load(file)
    except (OSError, ValueError):
        return default


def save_json(path, value):
    os.makedirs(os.path.dirname(path), exist_ok=True)
    temporary = f"{path}.{os.getpid()}.tmp"
    with open(temporary, "w", encoding="utf-8") as file:
        json.dump(value, file)
    os.replace(temporary, path)


def repo_root(folder):
    folder = os.path.abspath(folder)
    while True:
        if os.path.exists(os.path.join(folder, ".git")):
            return folder
        parent = os.path.dirname(folder)
        if parent == folder:
            return None
        folder = parent


def cache_path(root, suffix):
    return os.path.join(CACHE_DIR, hashlib.sha1(root.encode()).hexdigest()[:16] + suffix)


def run(command, cwd=None, timeout=20):
    """Standard output, standard error and whether it succeeded."""
    try:
        done = subprocess.run(command, cwd=cwd, capture_output=True, text=True, timeout=timeout)
        return done.stdout, done.stderr, done.returncode == 0
    except (OSError, subprocess.TimeoutExpired) as error:
        return "", str(error), False


def parse_time(text):
    if not text:
        return None
    try:
        return dt.datetime.fromisoformat(text.replace("Z", "+00:00")).timestamp()
    except ValueError:
        return None


# --- GitHub -----------------------------------------------------------------


def github_repository(root):
    url, _, ok = run(["git", "-C", root, "config", "--get", "remote.origin.url"])
    match = re.search(r"github\.com[:/]([^/]+)/(.+?)(?:\.git)?/?$", url.strip()) if ok else None
    return f"{match.group(1)}/{match.group(2)}" if match else None


def gh(path, token=None):
    """The reply as JSON, None when GitHub says there is none (404), or an
    exception naming what went wrong. With a token, as that token rather
    than gh's login."""
    command = ["gh", "api", path]
    if token:
        command = ["env", f"GH_TOKEN={token}"] + command
    out, err, ok = run(command)
    if ok:
        return json.loads(out) if out.strip() else None
    if '"user" scope' in err or "HTTP 403" in err:
        raise PermissionError("needs the user scope")
    if "HTTP 404" in err:
        return None
    raise RuntimeError(err.strip().splitlines()[-1] if err.strip() else "gh failed")


def workflow_triggers(root, path):
    """The events a workflow file starts on, as written in its "on:"."""
    try:
        with open(os.path.join(root, path), encoding="utf-8") as file:
            text = file.read()
    except OSError:
        return set()
    # Strip comments, so a trigger commented out doesn't count.
    lines = [re.sub(r"(^|\s)#.*$", "", line).rstrip() for line in text.splitlines()]
    events = {}  # each event, and the lines written under it
    for index, line in enumerate(lines):
        match = re.match(r"""^["']?on["']?:\s*(.*)$""", line)
        if not match:
            continue
        if match.group(1).strip():  # on: push, or on: [push, pull_request]
            events = {name: "" for name in re.findall(r"[a-z_]+", match.group(1))}
            break
        block = []
        for body in lines[index + 1:]:
            if body and not body[0].isspace():
                break
            if body.strip():
                block.append(body)
        if block:
            indent = min(len(body) - len(body.lstrip()) for body in block)
            current = None
            for body in block:
                if len(body) - len(body.lstrip()) == indent:
                    current = body.strip().lstrip("- ").split(":")[0].strip("'\"")
                    events[current] = ""
                elif current:
                    events[current] += body.strip() + "\n"
        break
    found = set()
    for name, detail in events.items():
        if name == "push":
            tags_only = bool(re.search(r"^tags", detail, re.M)) and not re.search(r"^branches", detail, re.M)
            found.add("tags" if tags_only else "push")
        elif name in ("pull_request", "pull_request_target"):
            found.add("PR")
        elif name == "schedule":
            found.add("schedule")
    return found


def describe_triggers(triggers, any_enabled):
    """What starts a run, in a fixed order: push, tags, PR, schedule; or
    ["manual"] when only a person can, or ["off"] when nothing is enabled."""
    if not any_enabled:
        return ["off"]
    found = [name for name in TRIGGERS if name in triggers and name not in ("manual", "off")]
    return found or ["manual"]


def fetch_github(root, repository):
    data = {"repository": repository}
    info = gh(f"repos/{repository}")
    data["private"] = bool(info and info.get("private"))

    # The newest tag, and the newest GitHub release when there is one.
    out, _, ok = run(["git", "-C", root, "for-each-ref", "--sort=-creatordate", "--count=1",
                      "--format=%(refname:short) %(creatordate:unix)", "refs/tags"])
    tag, _, made = out.strip().partition(" ") if ok else ("", "", "")
    if tag:
        data["tag"] = {"name": tag, "url": f"https://github.com/{repository}/tree/{tag}",
                       "time": float(made) if made.isdigit() else None}
    release = gh(f"repos/{repository}/releases/latest")
    if release:
        data["release"] = {"name": release.get("tag_name"), "url": release.get("html_url"),
                           "time": parse_time(release.get("published_at"))}

    runs = (gh(f"repos/{repository}/actions/runs?per_page=1") or {}).get("workflow_runs") or []
    if runs:
        latest = runs[0]
        data["run"] = {"status": latest.get("status"), "conclusion": latest.get("conclusion"),
                       "url": latest.get("html_url"), "name": latest.get("name"),
                       "time": parse_time(latest.get("updated_at") or latest.get("created_at"))}

    workflows = (gh(f"repos/{repository}/actions/workflows?per_page=100") or {}).get("workflows") or []
    triggers, enabled = set(), False
    for workflow in workflows:
        path = workflow.get("path", "")
        if workflow.get("state") != "active" or not path.startswith(".github/"):
            continue  # disabled, or made by GitHub itself (Dependabot)
        if not os.path.exists(os.path.join(root, path)):
            continue  # no longer in this checkout
        enabled = True
        triggers |= workflow_triggers(root, path)
    if workflows:
        data["triggers"] = describe_triggers(triggers, enabled)
    return data


def billing_token():
    out, _, ok = run(["security", "find-generic-password", "-s", "claude-statusline-github", "-w"])
    return out.strip() if ok and out.strip() else None


def fetch_billing():
    token = billing_token()
    if not token:
        raise PermissionError("no billing token in the keychain")
    user = gh("user", token) or {}
    login = user.get("login")
    plan = ((user.get("plan") or {}).get("name") or "").lower()
    now = dt.datetime.now(dt.timezone.utc)
    summary = gh(f"users/{login}/settings/billing/usage/summary?year={now.year}&month={now.month}", token)
    if summary is None:
        raise PermissionError("the token cannot read billing")
    summary = summary or {}
    minutes = free_used = spent = 0.0
    for item in summary.get("usageItems") or []:
        if (item.get("product") or "").lower() != "actions":
            continue
        spent += item.get("netAmount") or 0
        if item.get("unitType", "").lower() != "minutes":
            continue
        quantity = item.get("grossQuantity") or 0
        minutes += quantity
        # The free part GitHub reports is in each system's own minutes, but the
        # allowance counts in Linux minutes, so a macOS one uses ten.
        sku = (item.get("sku") or "").lower()
        multiplier = next((m for os_, m in MINUTE_MULTIPLIER.items() if os_ in sku), 1)
        free = item.get("discountQuantity")
        free_used += (quantity if free is None else free) * multiplier
    return {"minutes": minutes, "freeUsed": free_used, "spent": spent,
            "freeTotal": FREE_MINUTES.get(plan, FREE_MINUTES["free"])}


# --- App Store Connect ------------------------------------------------------


def base64url(data):
    return base64.urlsafe_b64encode(data).rstrip(b"=").decode()


def der_to_raw(signature):
    """An ECDSA signature as openssl writes it (DER) to the 64 bytes a JWT holds."""
    def integer(at):
        assert signature[at] == 0x02
        length = signature[at + 1]
        value = signature[at + 2:at + 2 + length]
        return value.lstrip(b"\0").rjust(32, b"\0"), at + 2 + length
    at = 3 if signature[1] & 0x80 else 2
    r, at = integer(at)
    s, _ = integer(at)
    return r + s


class AppStoreConnect:
    def __init__(self, key_id, issuer_id):
        self.key_path = os.path.join(HOME, ".appstoreconnect", "private_keys", f"AuthKey_{key_id}.p8")
        now = int(time.time())
        header = base64url(json.dumps({"alg": "ES256", "kid": key_id, "typ": "JWT"}).encode())
        claims = base64url(json.dumps({"iss": issuer_id, "iat": now, "exp": now + 1200,
                                       "aud": "appstoreconnect-v1"}).encode())
        signing = f"{header}.{claims}"
        done = subprocess.run(["openssl", "dgst", "-sha256", "-sign", self.key_path],
                              input=signing.encode(), capture_output=True, timeout=20)
        if done.returncode:
            raise RuntimeError("could not sign with the App Store Connect key")
        self.token = f"{signing}.{base64url(der_to_raw(done.stdout))}"

    def get(self, path):
        request = urllib.request.Request("https://api.appstoreconnect.apple.com" + path,
                                         headers={"Authorization": f"Bearer {self.token}"})
        try:
            with urllib.request.urlopen(request, timeout=20) as response:
                return json.load(response)
        except urllib.error.HTTPError as error:
            if error.code == 404:
                return None
            raise


def bundle_ids(root):
    found = []
    for project in glob.glob(os.path.join(root, "*.xcodeproj", "project.pbxproj")) + \
            glob.glob(os.path.join(root, "*", "*.xcodeproj", "project.pbxproj")):
        try:
            with open(project, encoding="utf-8") as file:
                text = file.read()
        except OSError:
            continue
        for value in re.findall(r'PRODUCT_BUNDLE_IDENTIFIER = "?([^";]+)"?;', text):
            if "$" not in value and value not in found:
                found.append(value)
    return found


def fetch_apple(root, config, previous):
    keys = config.get("appStoreConnect") or {}
    if not (keys.get("keyID") and keys.get("issuerID")):
        return {"error": "no App Store Connect key in ~/.claude/statusline-services.json"}
    api = AppStoreConnect(keys["keyID"], keys["issuerID"])

    app_id = previous.get("appID")
    if not app_id:
        ids = bundle_ids(root)
        apps = (api.get(f"/v1/apps?filter[bundleId]={','.join(ids)}&fields[apps]=bundleId") or {}).get("data") or []
        if not apps:
            return {"none": True}  # not an app this key can see
        app_id = apps[0]["id"]
    data = {"appID": app_id, "teamID": keys["issuerID"]}  # App Store Connect's pages name the team by it

    # Xcode Cloud: the latest run, what starts one, and this month's compute.
    product = (api.get(f"/v1/apps/{app_id}/ciProduct") or {}).get("data")
    if product:
        data["cloudProduct"] = product["id"]
        workflows =(api.get(f"/v1/ciProducts/{product['id']}/workflows") or {}).get("data") or []
        triggers, enabled = set(), False
        for workflow in workflows:
            attributes = workflow["attributes"]
            if not attributes.get("isEnabled"):
                continue
            enabled = True
            for field, name in (("branchStartCondition", "push"), ("tagStartCondition", "tags"),
                                ("pullRequestStartCondition", "PR"), ("scheduledStartCondition", "schedule")):
                if attributes.get(field):
                    triggers.add(name)
        data["cloudTriggers"] = describe_triggers(triggers, enabled)

        runs = (api.get(f"/v1/ciProducts/{product['id']}/buildRuns?sort=-number&limit=50"
                        "&fields[ciBuildRuns]=number,createdDate,startedDate,finishedDate,"
                        "executionProgress,completionStatus") or {}).get("data") or []
        if runs:
            latest = runs[0]["attributes"]
            data["cloudRun"] = {"progress": latest.get("executionProgress"),
                                "status": latest.get("completionStatus"),
                                "time": parse_time(latest.get("finishedDate") or latest.get("startedDate")
                                                   or latest.get("createdDate"))}
        month = dt.datetime.now(dt.timezone.utc).strftime("%Y-%m")
        durations = dict(previous.get("runSeconds") or {})  # finished runs never change
        total = 0.0
        for each in runs:
            attributes = each["attributes"]
            if not (attributes.get("createdDate") or "").startswith(month):
                continue
            if each["id"] not in durations:
                actions = (api.get(f"/v1/ciBuildRuns/{each['id']}/actions"
                                   "?fields[ciBuildActions]=startedDate,finishedDate&limit=50") or {}).get("data") or []
                seconds = 0.0
                for action in actions:
                    start = parse_time(action["attributes"].get("startedDate"))
                    end = parse_time(action["attributes"].get("finishedDate")) or time.time()
                    if start:
                        seconds += max(0.0, end - start)
                if attributes.get("executionProgress") != "COMPLETE":
                    total += seconds  # still running: count it, but fetch it again next time
                    continue
                durations[each["id"]] = seconds
            total += durations[each["id"]]
        data["runSeconds"] = {key: value for key, value in durations.items()
                              if key in {each["id"] for each in runs}}
        data["cloudHours"] = total / 3600

    # TestFlight: the newest build, and the newest each group of testers has.
    reply = api.get(f"/v1/builds?filter[app]={app_id}&sort=-uploadedDate&limit=50"
                    "&include=preReleaseVersion,buildBetaDetail"
                    "&fields[builds]=version,uploadedDate,processingState,expired,preReleaseVersion,buildBetaDetail"
                    "&fields[preReleaseVersions]=version"
                    "&fields[buildBetaDetails]=internalBuildState,externalBuildState") or {}
    included = {(item["type"], item["id"]): item["attributes"] for item in reply.get("included") or []}
    builds = []
    for build in reply.get("data") or []:
        relation = build.get("relationships") or {}
        version = included.get(("preReleaseVersions", ((relation.get("preReleaseVersion") or {}).get("data") or {}).get("id")), {})
        beta = included.get(("buildBetaDetails", ((relation.get("buildBetaDetail") or {}).get("data") or {}).get("id")), {})
        attributes = build["attributes"]
        builds.append({"id": build["id"], "version": version.get("version"), "build": attributes.get("version"),
                       "processing": attributes.get("processingState"), "expired": attributes.get("expired"),
                       "internal": beta.get("internalBuildState"), "external": beta.get("externalBuildState"),
                       "time": parse_time(attributes.get("uploadedDate"))})
    if builds:
        data["newestBuild"] = builds[0]
        data["internal"] = next((b for b in builds if b["internal"] == "IN_BETA_TESTING"), None)
        data["external"] = next((b for b in builds if b["external"] == "IN_BETA_TESTING"), None)
        # Only builds sent to Apple for review: every build can be sent, so "not submitted" says nothing.
        data["externalNewest"] = next((b for b in builds if b["external"] in (
            "WAITING_FOR_BETA_REVIEW", "IN_BETA_REVIEW", "BETA_REJECTED", "BETA_APPROVED", "IN_BETA_TESTING")), None)

    # The App Store: the version on sale, and the newest one since.
    versions = (api.get(f"/v1/apps/{app_id}/appStoreVersions?limit=20"
                        "&fields[appStoreVersions]=versionString,appStoreState,appVersionState,createdDate") or {}).get("data") or []
    versions = sorted((v["attributes"] for v in versions), key=lambda v: v.get("createdDate") or "", reverse=True)
    state = lambda v: v.get("appVersionState") or v.get("appStoreState")
    live = next((v for v in versions if state(v) in ("READY_FOR_DISTRIBUTION", "READY_FOR_SALE")), None)
    if live:
        data["live"] = live.get("versionString")
    pending = next((v for v in versions if state(v) not in
                    ("READY_FOR_DISTRIBUTION", "READY_FOR_SALE", "REPLACED_WITH_NEW_VERSION")), None)
    if pending and (not live or (pending.get("createdDate") or "") > (live.get("createdDate") or "")):
        submissions = (api.get(f"/v1/reviewSubmissions?filter[app]={app_id}&limit=10"
                               "&fields[reviewSubmissions]=submittedDate,state") or {}).get("data") or []
        submitted = max((parse_time(s["attributes"].get("submittedDate")) or 0 for s in submissions), default=0)
        data["pending"] = {"version": pending.get("versionString"), "state": state(pending),
                           "time": submitted or parse_time(pending.get("createdDate"))}
    return data


# --- Refreshing -------------------------------------------------------------


def refresh(root):
    lock = cache_path(root, ".lock")
    config = load_json(CONFIG, {})
    cache = load_json(cache_path(root, ".json"), {})
    now = time.time()
    try:
        repository = github_repository(root)
        if repository and now - cache.get("github", {}).get("fetched", 0) > TTL["github"]:
            try:
                cache["github"] = fetch_github(root, repository)
            except Exception as error:  # keep what was there, and say it is old
                cache.setdefault("github", {})["error"] = str(error)
            cache["github"]["fetched"] = now
        if repository and now - cache.get("billing", {}).get("fetched", 0) > TTL["billing"]:
            try:
                cache["billing"] = fetch_billing()
            except PermissionError:
                cache["billing"] = {"needsScope": True}
            except Exception as error:
                cache.setdefault("billing", {})["error"] = str(error)
            cache["billing"]["fetched"] = now
        if bundle_ids(root) and now - cache.get("apple", {}).get("fetched", 0) > TTL["apple"]:
            previous = cache.get("apple", {})
            try:
                cache["apple"] = fetch_apple(root, config, previous)
            except Exception as error:
                previous["error"] = str(error)
                cache["apple"] = previous
            cache["apple"]["fetched"] = now
        cache["nerdFont"] = any("nerd" in name.lower() for folder in
                                (os.path.join(HOME, "Library", "Fonts"), "/Library/Fonts")
                                if os.path.isdir(folder) for name in os.listdir(folder))
        save_json(cache_path(root, ".json"), cache)
    finally:
        try:
            os.remove(lock)
        except OSError:
            pass


def start_refresh(root, cache):
    now = time.time()
    stale = any(now - cache.get(part, {}).get("fetched", 0) > ttl for part, ttl in TTL.items())
    if not stale:
        return
    lock = cache_path(root, ".lock")
    try:
        if now - os.path.getmtime(lock) < LOCK_TIMEOUT:
            return  # one is already running
    except OSError:
        pass
    os.makedirs(CACHE_DIR, exist_ok=True)
    with open(lock, "w"):
        pass
    subprocess.Popen([sys.executable, os.path.abspath(__file__), "--refresh", root],
                     stdin=subprocess.DEVNULL, stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL,
                     start_new_session=True)


# --- Printing ---------------------------------------------------------------


# Each mark as a Nerd Font glyph, and as text without one.
ICONS = {
    "github": ("", "GitHub"),  # fa-github
    "apple": ("", ""),  # fa-apple; without a Nerd Font, the logo Apple's own fonts have
    "actions": ("", "CI"),  # cod-play_circle, as GitHub draws Actions
    "release": ("", "release"),  # oct-package: a GitHub release, beside the newest tag
    # What starts a run.
    "on-push": ("", "push"),  # oct-git_branch
    "on-tags": ("", "tags"),  # oct-tag
    "on-pr": ("", "PR"),  # oct-git_pull_request
    "on-schedule": ("", "schedule"),  # oct-clock
    "on-manual": ("", "manual"),  # fa-hand_pointer: only when started by hand
    "off": ("", "off"),  # oct-circle_slash: nothing enabled
    "cloud": ("", "Xcode Cloud"),  # fa-cloud
    "testflight": ("", "TestFlight"),  # fa-plane: Nerd Fonts have no TestFlight logo
    "appstore": ("", "App Store"),  # fa-app_store_ios
}
# Each trigger, in the order shown, and its icon.
TRIGGERS = {"push": "on-push", "tags": "on-tags", "PR": "on-pr", "schedule": "on-schedule",
            "manual": "on-manual", "off": "off"}
GITHUB_USAGE ="https://github.com/settings/billing"  # the overview, with a tab for Actions
GITHUB_BUDGETS = "https://github.com/settings/billing/budgets"


def ago(moment):
    if not moment:
        return ""
    seconds = max(0, time.time() - moment)
    if seconds < 3600:
        return f"{max(1, int(seconds // 60))}m"
    if seconds < 86400:
        return f"{int(seconds // 3600)}h"
    return f"{int(seconds // 86400)}d"


def link(text, url):
    return f"\033]8;;{url}\033\\{text}\033]8;;\033\\" if url else text


# Between a line's logo and what it says.
LOGO_BREAK = f"{DIM}»{RESET} "  # after icon_link's space


def icon_link(mark, url):
    """A link that is only an icon. The icons are wider than a character and
    spill over the space after them, so the space goes in the link too, or
    the icon's right half would not be clickable."""
    return link(f"{mark} ", url)


def icon_text_link(mark, text, url):
    """A link that is an icon and then text."""
    return link(f"{mark} {text}", url)


def sep():
    return f" {DIM}·{RESET} "


def build_name(build):
    return f"{build['version']} ({build['build']})"


def ci_mark(good, running):
    if running:
        return f"{YELLOW}⟳{RESET}"
    if good is None:
        return f"{DIM}⊘{RESET}"
    return f"{GREEN}✓{RESET}" if good else f"{RED}✗{RESET}"


def show_triggers(triggers, icon):
    """The icons for what starts a run, or its words without a Nerd Font."""
    if isinstance(triggers, str):  # saved by an older version of this script
        return f"{DIM}{triggers}{RESET}"
    marks = [icon(TRIGGERS[name]) for name in triggers if name in TRIGGERS]
    return f"{DIM}{' '.join(marks)}{RESET}"


def allowance(used, total, unit, approximate=False):
    """The free allowance used, coloured by how much of it is gone."""
    share = used / total if total else 0
    colour = RED if share >= 0.9 else YELLOW if share >= 0.7 else GREEN
    about = "≈" if approximate else ""
    return f"{colour}{about}{used:.0f}{RESET}{DIM}/{total}{unit} free{RESET}" if unit == "m" else \
        f"{colour}{about}{used:.1f}{RESET}{DIM}/{total}{unit} free{RESET}"


# The order on both lines: the latest run, what starts one, the free
# allowance used (and GitHub's spend), then the releases.
def show_github(cache, icon):
    github = cache.get("github") or {}
    if not github.get("repository"):
        return None
    parts = []
    if github.get("run"):
        run_ = github["run"]
        running = run_["status"] != "completed"
        good = None if run_["conclusion"] in ("cancelled", "skipped") else run_["conclusion"] == "success"
        parts.append(icon_text_link(icon("actions"), f"{ci_mark(good, running)} {DIM}{ago(run_['time'])}{RESET}",
                                    run_.get("url")))
    if github.get("triggers"):
        parts.append(show_triggers(github["triggers"], icon))
    billing = cache.get("billing") or {}
    if not github.get("private"):
        parts.append(f"{DIM}public: Actions free{RESET}")
    elif billing.get("needsScope"):
        parts.append(f"{DIM}minutes: no billing token{RESET}")
    elif "minutes" in billing:
        parts.append(link(f"{allowance(billing['freeUsed'], billing['freeTotal'], 'm')} "
                          f"{DIM}({billing['minutes']:.0f}m run){RESET}", GITHUB_USAGE))
        budget = load_json(CONFIG, {}).get("githubBudgetDollars")
        spent = billing["spent"]
        if budget is not None:
            # Coloured as the allowances are. The budget is typed into the config, not
            # read from GitHub, so a "?" says it may be out of date.
            share = spent / budget if budget else (1 if spent > 0 else 0)
            colour = RED if share >= 0.9 else YELLOW if share >= 0.7 else GREEN
            parts.append(link(f"{colour}${spent:.2f}{RESET}{DIM}/${budget}?{RESET}", GITHUB_BUDGETS))
        else:
            parts.append(link(f"${spent:.2f}", GITHUB_BUDGETS))
    if github.get("tag"):
        tag = github["tag"]
        parts.append(icon_text_link(f"{DIM}{icon('on-tags')}{RESET}", f"{tag['name']} {DIM}{ago(tag.get('time'))}{RESET}",
                          tag.get("url")))
    if github.get("release"):
        release = github["release"]
        parts.append(icon_text_link(f"{DIM}{icon('release')}{RESET}",
                                    f"{release['name']} {DIM}{ago(release.get('time'))}{RESET}",
                          release.get("url")))
    if github.get("error"):
        parts.append(f"{YELLOW}!{RESET}{DIM} {ago(github.get('fetched'))} old{RESET}")
    project = f"https://github.com/{github['repository']}"
    return icon_link(f"{BOLD}{icon('github')}{RESET}", project) + LOGO_BREAK + sep().join(parts)


# What each state is called, and its colour.
TESTFLIGHT_STATES = {
    "PROCESSING": ("processing", YELLOW), "PROCESSING_EXCEPTION": ("processing failed", RED),
    "MISSING_EXPORT_COMPLIANCE": ("needs export compliance", RED),
    "READY_FOR_BETA_TESTING": ("ready, no testers", YELLOW), "IN_BETA_TESTING": ("✓", GREEN),
    "EXPIRED": ("expired", DIM), "READY_FOR_BETA_SUBMISSION": ("not submitted", DIM),
    "IN_EXPORT_COMPLIANCE_REVIEW": ("in export compliance review", YELLOW),
    "WAITING_FOR_BETA_REVIEW": ("waiting for review", YELLOW), "IN_BETA_REVIEW": ("in review", YELLOW),
    "BETA_REJECTED": ("rejected", RED), "BETA_APPROVED": ("approved, not sent", YELLOW),
}
STORE_STATES = {
    "PREPARE_FOR_SUBMISSION": ("not submitted", DIM), "READY_FOR_REVIEW": ("not submitted", DIM),
    "WAITING_FOR_REVIEW": ("waiting for review", YELLOW), "IN_REVIEW": ("in review", YELLOW),
    "WAITING_FOR_EXPORT_COMPLIANCE": ("needs export compliance", RED),
    "PENDING_DEVELOPER_RELEASE": ("approved, release it", GREEN),
    "PENDING_APPLE_RELEASE": ("approved, releasing", GREEN), "ACCEPTED": ("approved", GREEN),
    "PROCESSING_FOR_DISTRIBUTION": ("approved, processing", GREEN),
    "PROCESSING_FOR_APP_STORE": ("approved, processing", GREEN),
    "REJECTED": ("rejected", RED), "METADATA_REJECTED": ("rejected", RED),
    "INVALID_BINARY": ("invalid build", RED), "DEVELOPER_REJECTED": ("withdrawn", DIM),
}


def state_text(state, table):
    name, colour = table.get(state, ((state or "?").lower().replace("_", " "), DIM))
    return f"{colour}{name}{RESET}"


def show_apple(cache, icon):
    apple = cache.get("apple")
    if not apple or apple.get("none"):
        return None
    if "appID" not in apple:
        return f"{BOLD}{icon('apple')}{RESET} {DIM}{apple.get('error', 'not fetched yet')}{RESET}"
    app = f"https://appstoreconnect.apple.com/apps/{apple['appID']}"
    cloud = (f"https://appstoreconnect.apple.com/teams/{apple.get('teamID')}/xcode-cloud/products/"
             f"{apple.get('cloudProduct')}")
    parts = []
    if apple.get("cloudRun"):
        run_ = apple["cloudRun"]
        running = run_["progress"] != "COMPLETE"
        good = None if run_["status"] in ("CANCELED", "SKIPPED") else run_["status"] == "SUCCEEDED"
        parts.append(icon_text_link(icon("cloud"), f"{ci_mark(good, running)} {DIM}{ago(run_['time'])}{RESET}",
                          cloud + "/groups"))
    if apple.get("cloudTriggers"):
        parts.append(show_triggers(apple["cloudTriggers"], icon))
    if "cloudHours" in apple:
        parts.append(link(allowance(apple["cloudHours"], XCODE_CLOUD_HOURS, "h", approximate=True),
                          cloud + "/usage"))

    def build(found, mark, label=""):
        """A build, its state and how long since it was uploaded, linked to its page."""
        return link(f"{label}{build_name(found)} {mark} {DIM}{ago(found.get('time'))}{RESET}",
                    f"{app}/testflight/ios/{found['id']}" if found.get("id") else app + "/testflight/ios")

    testflight = []
    internal, newest = apple.get("internal"), apple.get("newestBuild")
    if internal:
        testflight.append(build(internal, f"{GREEN}✓{RESET}"))
    if newest and newest is not internal and (not internal or newest["build"] != internal["build"]):
        state = newest["processing"] if newest["processing"] != "VALID" else newest["internal"]
        testflight.append(build(newest, state_text(state, TESTFLIGHT_STATES)))
    external, external_newest = apple.get("external"), apple.get("externalNewest")
    if external:
        testflight.append(build(external, f"{GREEN}✓{RESET}", f"{DIM}external{RESET} "))
    if external_newest and (not external or external_newest["build"] != external["build"]):
        testflight.append(build(external_newest, state_text(external_newest["external"], TESTFLIGHT_STATES),
                                f"{DIM}external{RESET} "))
    if testflight:
        parts.append(icon_link(icon("testflight"), app + "/testflight/ios") + sep().join(testflight))

    # App Store Connect calls the version on sale "deliverable", and the one being made "inflight".
    store = []
    if apple.get("live"):
        store.append(link(f"{apple['live']} {GREEN}live{RESET}", app + "/distribution/ios/version/deliverable"))
    if apple.get("pending"):
        pending = apple["pending"]
        store.append(link(f"{pending['version']} {state_text(pending['state'], STORE_STATES)} "
                          f"{DIM}{ago(pending.get('time'))}{RESET}", app + "/distribution/ios/version/inflight"))
    if store:
        parts.append(icon_link(icon("appstore"), app + "/distribution") + sep().join(store))
    if apple.get("error"):
        parts.append(f"{YELLOW}!{RESET}{DIM} {ago(apple.get('fetched'))} old{RESET}")
    # The app's own page (…/apps/<id>) opens nothing, so the logo opens the list of apps.
    return icon_link(f"{BOLD}{icon('apple')}{RESET}", "https://appstoreconnect.apple.com/apps") + LOGO_BREAK \
        + sep().join(parts)


def show(folder):
    root = repo_root(folder)
    if not root:
        return
    cache = load_json(cache_path(root, ".json"), {})
    start_refresh(root, cache)
    icons = load_json(CONFIG, {}).get("icons", "auto")
    nerd = icons == "nerd" or (icons == "auto" and cache.get("nerdFont"))
    icon = lambda name: ICONS[name][0 if nerd else 1]
    lines = [show_github(cache, icon), show_apple(cache, icon)]
    for line in lines:  # each on a line of its own
        if line:
            print(line)


if __name__ == "__main__":
    if sys.argv[1:2] == ["--refresh"]:
        refresh(sys.argv[2])
    elif len(sys.argv) > 1:
        show(sys.argv[1])
