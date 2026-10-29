#!/bin/bash
# Status line converted from PS1:
# [\u@\h \W$(__git_ps1 " (%s)")]
# followed by the model, the context used, the prompt cache's state and
# the usage limits.
input=$(cat)
now=$(date +%s)

until_reset() {  # epoch seconds -> "42m", "2h 10m", "2h", "3d 4h" or "3d"
  local left=$(( $1 - now ))
  [ "$left" -le 0 ] && { echo "now"; return; }
  if [ "$left" -ge 86400 ]; then
    local hours=$((left % 86400 / 3600))
    [ "$hours" -eq 0 ] && echo "$((left / 86400))d" || echo "$((left / 86400))d ${hours}h"
  elif [ "$left" -ge 3600 ]; then
    local mins=$((left % 3600 / 60))
    [ "$mins" -eq 0 ] && echo "$((left / 3600))h" || echo "$((left / 3600))h ${mins}m"
  else echo "$(( (left + 59) / 60 ))m"; fi
}

# Colours. The reset also ends bold.
dim=$'\033[2m'; bold=$'\033[1m'; reset=$'\033[0m'
red=$'\033[31m'; green=$'\033[32m'; yellow=$'\033[33m'; blue=$'\033[34m'
magenta=$'\033[35m'; cyan=$'\033[36m'

cwd=$(echo "$input" | jq -r '.workspace.current_dir // .cwd')

# --- The prompt part: [user@host dir (branch*+)] ---------------------------
branch=$(git -C "$cwd" --no-optional-locks symbolic-ref --short HEAD 2>/dev/null \
  || git -C "$cwd" --no-optional-locks rev-parse --short HEAD 2>/dev/null)
# The flags as __git_ps1 writes them with every GIT_PS1_SHOW* option on:
# * unstaged, + staged, $ stashed, % untracked, after a space; then the
# upstream right after: = level, < behind, > ahead, <> diverged.
git_part=""
if [ -n "$branch" ]; then
  flags=""
  git -C "$cwd" --no-optional-locks diff --quiet 2>/dev/null || flags="*"
  git -C "$cwd" --no-optional-locks diff --cached --quiet 2>/dev/null || flags="${flags}+"
  [ -n "$(git -C "$cwd" --no-optional-locks stash list 2>/dev/null | head -1)" ] && flags="${flags}\$"
  [ -n "$(git -C "$cwd" --no-optional-locks ls-files --others --exclude-standard 2>/dev/null | head -1)" ] && flags="${flags}%"
  upstream=""
  counts=$(git -C "$cwd" --no-optional-locks rev-list --left-right --count '@{upstream}...HEAD' 2>/dev/null)
  if [ -n "$counts" ]; then
    behind=${counts%%[[:space:]]*}; ahead=${counts##*[[:space:]]}
    if [ "$behind" -eq 0 ] && [ "$ahead" -eq 0 ]; then upstream="="
    elif [ "$behind" -eq 0 ]; then upstream=">"
    elif [ "$ahead" -eq 0 ]; then upstream="<"
    else upstream="<>"; fi
  fi
  git_part=" ${magenta}(${branch}${red}${flags:+ $flags}${yellow}${upstream}${magenta})${reset}"
fi
prompt="${dim}[${reset}${green}$(whoami)@$(hostname -s)${reset} ${bold}${blue}$(basename "$cwd")${reset}${git_part}${dim}]${reset}"

# --- Model ------------------------------------------------------------------
model=$(echo "$input" | jq -r '.model.display_name // empty')
model_part=""
[ -n "$model" ] && model_part=" ${cyan}${model}${reset}"

# --- Context: used/size and a percentage, coloured by how full it is ---------
# Tokens as "75k" below a million, "1.2M" above. The percentage is the one
# Claude Code gives, or worked out from the token counts on older versions.
read -r used size pct < <(echo "$input" | jq -r '
  def k: if . >= 1000000 then ((. / 100000 | floor) / 10 | tostring) + "M"
         elif . >= 1000 then ((. / 1000 | round) | tostring) + "k"
         else tostring end;
  .context_window as $c
  | ($c.total_input_tokens
     // (($c.current_usage // {}) | ((.input_tokens // 0) + (.cache_creation_input_tokens // 0) + (.cache_read_input_tokens // 0)))) as $used
  | ($c.context_window_size // 0) as $size
  | ($c.used_percentage // (if $size > 0 then $used * 100 / $size else null end)) as $pct
  | if $c == null then empty
    else [($used | k), (if $size > 0 then ($size | k) else "?" end), (if $pct == null then "?" else ($pct | round | tostring) end)] | join(" ")
    end' 2>/dev/null)
context_part=""
if [ -n "$pct" ]; then
  colour=$green
  if [ "$pct" = "?" ]; then colour=$dim
  elif [ "$pct" -ge 90 ]; then colour=$red
  elif [ "$pct" -ge 70 ]; then colour=$yellow
  fi
  context_part=" ${dim}·${reset} ${colour}${used}/${size} ${pct}%${reset}"
fi

# --- Prompt cache: warm or cold, its lifetime, time left and hit ratio ------
# Absent until the first reply. "warm" is green while the cached prefix is
# within its lifetime, "cold" red once it has lapsed; the hit ratio is
# cache reads as a share of all input tokens this session.
read -r warm observed ttl expires ratio < <(echo "$input" | jq -r --argjson now "$now" '
  .prompt_cache // empty
  | [(.warm | tostring), (.caching_observed | tostring), (.ttl // "-"),
     (if .expires_at == null then "-" else (((.expires_at - $now) / 60) | ceil | tostring) end),
     (if .hit_ratio == null then "-" else ((.hit_ratio * 100) | round | tostring) end)]
  | join(" ")' 2>/dev/null)
cache_part=""
if [ -n "$warm" ]; then
  if [ "$observed" != "true" ]; then
    cache_part=" ${dim}· no cache${reset}"
  elif [ "$warm" = "true" ]; then
    # The lifetime (5m or 1h) in front, then the time left: "1h cache
    # warm 42m left", or "1h left" with the whole hour still to run.
    left=""
    [ "$expires" != "-" ] && [ "$expires" -gt 0 ] && left=" $(until_reset $((now + expires * 60))) left"
    cache_part=" ${dim}·${reset} ${dim}${ttl}${reset} ${green}cache warm${reset}${dim}${left}${reset}"
  else
    cache_part=" ${dim}·${reset} ${dim}${ttl}${reset} ${red}cache cold${reset}"
  fi
  [ "$ratio" != "-" ] && [ -n "$ratio" ] && cache_part="${cache_part}${dim} ${ratio}% hits${reset}"
fi

# --- Usage limits: the 5-hour and 7-day windows, and when each resets -----
# Only claude.ai Pro and Max accounts get them, after the first reply.
limits_part=""
while read -r label used resets; do
  [ -z "$label" ] && continue
  colour=$green
  if [ "$used" -ge 90 ]; then colour=$red; elif [ "$used" -ge 70 ]; then colour=$yellow; fi
  when=""
  [ "$resets" != "-" ] && when="${dim} ↻$(until_reset "$resets")${reset}"
  limits_part+=" ${dim}·${reset} ${colour}${label} ${used}%${reset}${when}"
done < <(echo "$input" | jq -r '
  .rate_limits // {} | to_entries[]
  | select(.key == "five_hour" or .key == "seven_day")
  | [(if .key == "five_hour" then "5h" else "7d" end),
     (.value.used_percentage // 0 | round | tostring),
     (.value.resets_at // "-" | tostring)] | join(" ")' 2>/dev/null)

# No nyan cat: Claude Code lays the status line out cell by cell and keeps
# only colours and links from the script's output, so a picture cannot be
# shown, and a text cat was not wanted.

printf '%s%s%s%s%s\n' "$prompt" "$model_part" "$context_part" "$cache_part" "$limits_part"

# --- Second line: GitHub, Xcode Cloud, TestFlight and the App Store ---------
# From a cache that statusline-services.py refreshes in the background, so
# this never waits on the network; nothing is printed outside such a project.
python3 ~/.claude/statusline-services.py "$cwd" 2>/dev/null
