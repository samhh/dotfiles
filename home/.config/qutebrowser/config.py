config.load_autoconfig()

# At time of writing this matches the Alacritty config, both in terms of
# (default) background colour and alpha (92%)
color_norm = "#EB1D1F21"
color_bold = "#EB0F0F0F"
color_noop = "#00000000"

c.colors.statusbar.normal.bg = color_norm
c.colors.statusbar.url.success.http.fg = "yellow"
c.colors.statusbar.url.success.https.fg = "white"
c.colors.tabs.bar.bg = color_noop
c.colors.tabs.even.bg = color_norm
c.colors.tabs.odd.bg = color_norm
c.colors.tabs.selected.even.bg = color_bold
c.colors.tabs.selected.odd.bg = color_bold
c.colors.webpage.darkmode.enabled = True
c.completion.cmd_history_max_items = 0
c.completion.open_categories = []
c.completion.web_history.max_items = 0
c.confirm_quit = ["downloads"]
c.content.blocking.enabled = False
c.content.cookies.accept = "no-3rdparty"
c.content.javascript.enabled = False
c.content.notifications.enabled = False
c.editor.command = "alacritty -e nvim {}".split()
c.scrolling.bar = "never"
c.scrolling.smooth = True
c.tabs.last_close = "close"
c.tabs.position = "bottom"
c.tabs.show = "multiple"
c.url.default_page = "about:blank"
c.url.searchengines = { "DEFAULT": "http://localhost:1234/?q={}" }
c.window.transparent = True

config.bind('X', 'spawn --userscript qute-pass --username-target secret --username-pattern "username: (.+)"')
config.bind('V', 'spawn streamlink {url}')
config.unbind('b')
config.unbind('B')
config.unbind('m')
config.unbind('M')

