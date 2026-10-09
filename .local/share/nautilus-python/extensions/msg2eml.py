import os
from gi.repository import Nautilus, GObject, Gio, GLib


class Batch:
    """Convert .msg files asynchronously, then send one summary notification."""

    def __init__(self, paths):
        self.pending = len(paths)
        self.ok = 0
        self.errors = []
        for p in paths:
            launcher = Gio.SubprocessLauncher.new(
                Gio.SubprocessFlags.STDOUT_PIPE | Gio.SubprocessFlags.STDERR_MERGE)
            launcher.set_cwd(os.path.dirname(p))      # msgconvert writes to cwd
            try:
                proc = launcher.spawnv(['msgconvert', p])
            except GLib.Error as e:                    # e.g. msgconvert not installed
                self.finish(p, False, e.message)
                continue
            proc.communicate_async(None, None, self.done, p)

    def done(self, proc, result, path):
        try:
            _, out, _ = proc.communicate_finish(result)
            text = out.get_data().decode(errors='replace') if out else ''
            self.finish(path, proc.get_successful(), text)
        except GLib.Error as e:
            self.finish(path, False, e.message)

    def finish(self, path, success, output):
        if success:
            self.ok += 1
        else:
            name = os.path.basename(path)
            self.errors.append(f"{name}: {output.strip() or 'unknown error'}")
        self.pending -= 1
        if self.pending == 0:
            self.notify()

    def notify(self):
        if self.errors:
            body = GLib.markup_escape_text('\n'.join(self.errors), -1)
            args = ['-u', 'critical', '-i', 'dialog-error',
                    f'msg2eml: {self.ok} converted, {len(self.errors)} failed', body]
        else:
            args = ['-i', 'mail-read', 'msg2eml', f'Converted {self.ok} file(s)']
        Gio.Subprocess.new(['notify-send', *args], Gio.SubprocessFlags.NONE)


class Msg2Eml(GObject.GObject, Nautilus.MenuProvider):
    def get_file_items(self, *args):          # works with old and new Nautilus APIs
        files = args[-1]
        paths = [f.get_location().get_path() for f in files
                 if f.get_uri_scheme() == 'file'
                 and f.get_name().lower().endswith('.msg')]
        if not paths:
            return []
        item = Nautilus.MenuItem(name='Msg2Eml::convert', label='Convert to .eml')
        item.connect('activate', lambda *_: Batch(paths))
        return [item]
