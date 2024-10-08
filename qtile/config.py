# Copyright (c) 2010 Aldo Cortesi
# Copyright (c) 2010, 2014 dequis
# Copyright (c) 2012 Randall Ma
# Copyright (c) 2012-2014 Tycho Andersen
# Copyright (c) 2012 Craig Barnes
# Copyright (c) 2013 horsik
# Copyright (c) 2013 Tao Sauvage
#
# Permission is hereby granted, free of charge, to any person obtaining a copy
# of this software and associated documentation files (the "Software"), to deal
# in the Software without restriction, including without limitation the rights
# to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
# copies of the Software, and to permit persons to whom the Software is
# furnished to do so, subject to the following conditions:
#
# The above copyright notice and this permission notice shall be included in
# all copies or substantial portions of the Software.
#
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
# IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
# FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
# AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
# LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
# OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
# SOFTWARE.

import re
import shlex
import subprocess

from libqtile import bar, layout, qtile, widget
from libqtile.config import Click, Drag, Group, Key, Match, Screen
from libqtile.lazy import lazy
from libqtile.utils import guess_terminal

mod = "mod4"  # super
alt = "mod1"
terminal = guess_terminal()


VOL_PATTERN = r"/\s*(\d+)%\s*/"
MUTE_PATTERN = r"Mute: (yes|no)"


def get_volume_and_mute(vol, mute):
    v = re.search(VOL_PATTERN, vol)
    vol = int(v[1]) if v else None

    m = re.search(MUTE_PATTERN, mute)
    mute = m[1] == "yes" if m else None
    return vol, mute


def build_volume_text(vol, mute):
    if vol is None or mute is None:
        return "???"
    return f"V {vol}%{' MUTE' if mute else ''}"


MUTE_GET_CMD = "pactl get-sink-mute @DEFAULT_SINK@"
MUTE_SET_CMD = "pactl set-sink-mute @DEFAULT_SINK@ toggle"

VOL_GET_CMD = "pactl get-sink-volume @DEFAULT_SINK@"
VOL_SET_CMD = "pactl set-sink-volume @DEFAULT_SINK@ {}%"


def pactl_get_volume_and_mute():
    return my_run(VOL_GET_CMD), my_run(MUTE_GET_CMD)


def pactl_toggle_mute():
    return my_run(MUTE_SET_CMD)


def pactl_set_volume(step):
    current, _ = get_volume_and_mute(*pactl_get_volume_and_mute())
    desired = current + step
    desired = min(desired, 100)
    desired = max(desired, 0)
    return my_run(VOL_SET_CMD.format(desired))


def my_run(cmd):
    p = subprocess.run(shlex.split(cmd), check=True, text=True, capture_output=True)
    return p.stdout


def my_mute(qtile):
    pactl_toggle_mute()
    text = build_volume_text(*get_volume_and_mute(*pactl_get_volume_and_mute()))
    qtile.widgets_map["vol"].update(text)


def my_volume_up(qtile):
    pactl_set_volume(10)
    text = build_volume_text(*get_volume_and_mute(*pactl_get_volume_and_mute()))
    qtile.widgets_map["vol"].update(text)


def my_volume_down(qtile):
    pactl_set_volume(-10)
    text = build_volume_text(*get_volume_and_mute(*pactl_get_volume_and_mute()))
    qtile.widgets_map["vol"].update(text)


def get_brightness(brightness):
    fields = brightness.split(",")
    if len(fields) != 5:
        return None
    b = fields[3]
    print(fields, b)
    b = b.strip()
    if not b.endswith("%"):
        return None
    b = b[0:-1]
    return int(b)


def build_brightness_text(brightness):
    if brightness is None:
        return "???"
    return f"B {brightness}%"


BRIGHTNESS_GET_CMD = "brightnessctl -m i"
BRIGHTNESS_SET_CMD = "brightnessctl -m s {}"


def brightness_get():
    return my_run(BRIGHTNESS_GET_CMD)


def brightness_set(step):
    step_text = f"+{step}%" if step >= 0 else f"{abs(step)}%-"
    return my_run(BRIGHTNESS_SET_CMD.format(step_text))


def my_brightness_up(qtile):
    brightness_set(10)
    text = build_brightness_text(get_brightness(brightness_get()))
    qtile.widgets_map["bright"].update(text)


def my_brightness_down(qtile):
    brightness_set(-10)
    text = build_brightness_text(get_brightness(brightness_get()))
    qtile.widgets_map["bright"].update(text)


keys = [
    # A list of available commands that can be bound to keys can be found
    # at https://docs.qtile.org/en/latest/manual/config/lazy.html
    # Switch between windows
    Key([mod, alt], "h", lazy.layout.left(), desc="Move focus to left"),
    Key([mod, alt], "l", lazy.layout.right(), desc="Move focus to right"),
    Key([mod, alt], "j", lazy.layout.down(), desc="Move focus down"),
    Key([mod, alt], "k", lazy.layout.up(), desc="Move focus up"),
    Key([mod], "space", lazy.layout.next(), desc="Move window focus to other window"),
    # Move windows between left/right columns or move up/down in current stack.
    # Moving out of range in Columns layout will create new column.
    Key(
        [mod, "shift"], "h", lazy.layout.shuffle_left(), desc="Move window to the left"
    ),
    Key(
        [mod, "shift"],
        "l",
        lazy.layout.shuffle_right(),
        desc="Move window to the right",
    ),
    Key([mod, "shift"], "j", lazy.layout.shuffle_down(), desc="Move window down"),
    Key([mod, "shift"], "k", lazy.layout.shuffle_up(), desc="Move window up"),
    # Grow windows. If current window is on the edge of screen and direction
    # will be to screen edge - window would shrink.
    Key([mod, "control"], "h", lazy.layout.grow_left(), desc="Grow window to the left"),
    Key(
        [mod, "control"], "l", lazy.layout.grow_right(), desc="Grow window to the right"
    ),
    Key([mod, "control"], "j", lazy.layout.grow_down(), desc="Grow window down"),
    Key([mod, "control"], "k", lazy.layout.grow_up(), desc="Grow window up"),
    Key([mod], "n", lazy.layout.normalize(), desc="Reset all window sizes"),
    # Toggle between split and unsplit sides of stack.
    # Split = all windows displayed
    # Unsplit = 1 window displayed, like Max layout, but still with
    # multiple stack panes
    Key(
        [mod, "shift"],
        "Return",
        lazy.layout.toggle_split(),
        desc="Toggle between split and unsplit sides of stack",
    ),
    Key([mod], "Return", lazy.spawn("alacritty -e fish"), desc="Launch terminal"),
    # Toggle between different layouts as defined below
    Key([mod], "Tab", lazy.next_layout(), desc="Toggle between layouts"),
    Key([mod], "w", lazy.window.kill(), desc="Kill focused window"),
    Key(
        [mod],
        "f",
        lazy.window.toggle_fullscreen(),
        desc="Toggle fullscreen on the focused window",
    ),
    Key(
        [mod],
        "t",
        lazy.window.toggle_floating(),
        desc="Toggle floating on the focused window",
    ),
    Key([mod, "shift"], "r", lazy.reload_config(), desc="Reload the config"),
    Key([mod, "shift"], "q", lazy.shutdown(), desc="Shutdown Qtile"),
    Key(
        [mod],
        "q",
        lazy.spawn("alacritty -e my_shutdown.py"),
        desc="Sync and Shutdown computer",
    ),
    Key([mod], "r", lazy.spawncmd(), desc="Spawn a command using a prompt widget"),
    Key(
        [mod, "shift"],
        "7",
        lazy.spawncmd(),
        desc="Spawn a command using a prompt widget",
    ),
    Key([mod], "j", lazy.hide_show_bar(), desc="Toggles bar visibility"),
    Key([mod], "l", lazy.spawn("slock"), desc="Lock screen"),
    Key([], "XF86AudioMute", lazy.function(my_mute), desc="Mute volume"),
    Key(
        [],
        "XF86AudioLowerVolume",
        lazy.function(my_volume_down),
        desc="Lower volume",
    ),
    Key(
        [],
        "XF86AudioRaiseVolume",
        lazy.function(my_volume_up),
        desc="Raise volume",
    ),
    Key(
        [],
        "XF86MonBrightnessDown",
        lazy.function(my_brightness_down),
        desc="Lower brightness",
    ),
    Key(
        [],
        "XF86MonBrightnessUp",
        lazy.function(my_brightness_up),
        desc="Raise brightness",
    ),
]

# Add key bindings to switch VTs in Wayland.
# We can't check qtile.core.name in default config as it is loaded before qtile is started
# We therefore defer the check until the key binding is run by using .when(func=...)
for vt in range(1, 8):
    keys.append(
        Key(
            ["control", "mod1"],
            f"f{vt}",
            lazy.core.change_vt(vt).when(func=lambda: qtile.core.name == "wayland"),
            desc=f"Switch to VT{vt}",
        )
    )


groups = [Group(i) for i in "1234"]

for i in groups:
    keys.extend(
        [
            # mod + group number = switch to group
            Key(
                [mod],
                i.name,
                lazy.group[i.name].toscreen(),
                desc="Switch to group {}".format(i.name),
            ),
            # mod + shift + group number = switch to & move focused window to group
            Key(
                [mod, "shift"],
                i.name,
                lazy.window.togroup(i.name, switch_group=True),
                desc="Switch to & move focused window to group {}".format(i.name),
            ),
        ]
    )

layouts = [
    layout.Max(),
    layout.Columns(border_focus_stack=["#d75f5f", "#8f3d3d"], border_width=4),
]

widget_defaults = dict(
    font="IBM Plex Mono",
    fontsize=24,
    padding=6,
)
extension_defaults = widget_defaults.copy()

screens = [
    Screen(
        bottom=bar.Bar(
            [
                widget.CurrentLayout(),
                widget.GroupBox(),
                widget.Prompt(),
                widget.TaskList(),
                widget.TextBox(
                    build_volume_text(
                        *get_volume_and_mute(*pactl_get_volume_and_mute())
                    ),
                    name="vol",
                ),
                widget.TextBox(
                    build_brightness_text(get_brightness(brightness_get())),
                    name="bright",
                ),
                widget.Battery(
                    background="00422a",
                    low_background="#620f2a",
                    low_foreground="#ffffff",
                    low_percentage=0.2,
                ),
                widget.Clock(format="%Y-%m-%d %a %I:%M %p"),
                widget.QuickExit(default_text="[X]", countdown_format="[{}]"),
            ],
            48,
        ),
    ),
]

# Drag floating layouts.
mouse = [
    Drag(
        [mod],
        "Button1",
        lazy.window.set_position_floating(),
        start=lazy.window.get_position(),
    ),
    Drag(
        [mod], "Button3", lazy.window.set_size_floating(), start=lazy.window.get_size()
    ),
    Click([mod], "Button2", lazy.window.bring_to_front()),
]

dgroups_key_binder = None
dgroups_app_rules = []  # type: list
follow_mouse_focus = True
bring_front_click = False
floats_kept_above = True
cursor_warp = False
floating_layout = layout.Floating(
    float_rules=[
        # Run the utility of `xprop` to see the wm class and name of an X client.
        *layout.Floating.default_float_rules,
        Match(wm_class="confirmreset"),  # gitk
        Match(wm_class="makebranch"),  # gitk
        Match(wm_class="maketag"),  # gitk
        Match(wm_class="ssh-askpass"),  # ssh-askpass
        Match(title="branchdialog"),  # gitk
        Match(title="pinentry"),  # GPG key password entry
    ]
)
auto_fullscreen = True
focus_on_window_activation = "smart"
reconfigure_screens = True

# If things like steam games want to auto-minimize themselves when losing
# focus, should we respect this or not?
auto_minimize = True

# When using the Wayland backend, this can be used to configure input devices.
wl_input_rules = None

# xcursor theme (string or None) and size (integer) for Wayland backend
wl_xcursor_theme = None
wl_xcursor_size = 24

# XXX: Gasp! We're lying here. In fact, nobody really uses or cares about this
# string besides java UI toolkits; you can see several discussions on the
# mailing lists, GitHub issues, and other WM documentation that suggest setting
# this string if your java app doesn't work correctly. We may as well just lie
# and say that we're a working one by default.
#
# We choose LG3D to maximize irony: it is a 3D non-reparenting WM written in
# java that happens to be on java's whitelist.
wmname = "LG3D"
