# Documentation:
#   qute://help/configuring.html
#   qute://help/settings.html

# Uncomment this to still load settings configured via autoconfig.yml
config.load_autoconfig(False)

# Aliases for commands. The keys of the given dictionary are the
# aliases, while the values are the commands they map to.
# Type: Dict
c.aliases = {'q': 'quit', 'w': 'session-save', 'wq': 'quit --save'}

# Enable JavaScript.
# Type: Bool
config.set('content.javascript.enabled', True, 'file://*')

# Enable JavaScript.
# Type: Bool
config.set('content.javascript.enabled', True, 'chrome://*/*')

# Enable JavaScript.
# Type: Bool
config.set('content.javascript.enabled', True, 'qute://*/*')

# Position of the tab bar.
# Type: Position
# Valid values:
#   - top
#   - bottom
#   - left
#   - right
c.tabs.position = 'top'

# When to show the tab bar.
# Type: String
# Valid values:
#   - always: Always show the tab bar.
#   - never: Always hide the tab bar.
#   - multiple: Hide the tab bar if only one tab is open.
#   - switching: Show the tab bar when switching tabs.
c.tabs.show = 'always'

# URL segments where `:navigate increment/decrement` will search for a
# number.
# Type: FlagList
# Valid values:
#   - host
#   - port
#   - path
#   - query
#   - anchor
c.url.incdec_segments = ['path', 'query']

# Search engines which can be used via the address bar. Maps a search
# engine name (such as `DEFAULT`, or `ddg`) to a URL with a `{}`
# placeholder. The placeholder will be replaced by the search term, use
# `{{` and `}}` for literal `{`/`}` signs. The search engine named
# `DEFAULT` is used when `url.auto_search` is turned on and something
# else than a URL was entered to be opened. Other search engines can be
# used by prepending the search engine name to the search term, e.g.
# `:open google qutebrowser`.
# Type: Dict
c.url.searchengines = {'DEFAULT': 'https://kagi.com/search?q={}',
                       'dg': 'https://duckduckgo.com/?q={}',
                       'az': 'https://www.amazon.com/s/ref=nb_sb_noss_2?url=search-alias%3Daps&field-keywords={}',
                       'aw': 'https://wiki.archlinux.org/index.php?title=Special%3ASearch&search={}',
                       'gh': 'https://github.com/search?q={}',
                       'yt': 'https://www.youtube.com/results?search_query={}',
                       'kc': 'https://www.knifecenter.com/kc_new/store_store.html?usrsearch={}',
                       'bq': 'https://www.bladehq.com/?search={}',}

# Page(s) to open at the start.
# Type: List of FuzzyUrl, or FuzzyUrl
c.url.start_pages = 'qute://start/'

# Foreground color of selected odd tabs.
# Type: QtColor
c.colors.tabs.selected.odd.fg = 'white'

# Background color of selected odd tabs.
# Type: QtColor
c.colors.tabs.selected.odd.bg = 'blue'

# Foreground color of selected even tabs.
# Type: QtColor
c.colors.tabs.selected.even.fg = 'white'

# Background color of selected even tabs.
# Type: QtColor
c.colors.tabs.selected.even.bg = 'blue'

# Default monospace fonts. Whenever "monospace" is used in a font
# setting, it's replaced with the fonts listed here.
# Type: Font
# c.fonts.monospace = '"Triplicate T4c", "xos4 Terminus", Terminus, Monospace, "DejaVu Sans Mono", Monaco, "Bitstream Vera Sans Mono", "Andale Mono", "Courier New", Courier, "Liberation Mono", monospace, Fixed, Consolas, Terminal'

# Font used in the statusbar.
# Type: Font
c.fonts.statusbar = '16pt monospace'

# Font used in the tab bar.
# Type: QtFont
c.fonts.tabs.selected = '16pt monospace'
c.fonts.tabs.unselected = '16pt monospace'

# Type: Dict
c.bindings.key_mappings['<Escape>'] = '<Ctrl-g>'

# prev, next, or last-used
c.tabs.select_on_remove = 'last-used'

# config.bind('<key>', 'command', mode='mode')
# <Ctrl-x><Ctrl-f> for emacs style chords
config.bind('<Ctrl-s>', 'set-cmd-text /')
config.bind('<Ctrl-f>', 'tab-next')
config.bind('<Ctrl-b>', 'tab-prev')
config.bind('<Alt-b>', 'back')
config.bind('<Alt-w>', 'yank')
config.bind('<Ctrl-y>', 'open -- {clipboard}')
config.bind('<Ctrl-n>', 'scroll down')
config.bind('<Ctrl-p>', 'scroll up')
config.bind('<Ctrl-v>', 'scroll-page 0 1')
config.bind('<Alt-v>', 'scroll-page 0 -1')
config.bind('<right>', 'tab-next')
config.bind('<left>', 'tab-prev')
config.bind('g', 'reload')
config.bind('<Alt-,>', 'scroll-to-perc 0')
config.bind('<Alt-.>', 'scroll-to-perc 100')

c.content.tls.certificate_errors = 'block'
