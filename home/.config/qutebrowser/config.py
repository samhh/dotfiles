import json

config.load_autoconfig()

colors_handle = open("/home/sam/.cache/wal/colors.json")
colors = json.load(colors_handle)
colors_handle.close()

theme_handle = open("/home/sam/.cache/wal/_theme")
theme = theme_handle.read()[:-1]
theme_handle.close()

def with_alpha(a):
  def on_color(c):
    return "#" + a + c[1:]
  return on_color

translucent = with_alpha("DD")

color_bghl = colors["special"]["background"]
color_bg = translucent(color_bghl)
color_fghl = colors["colors"]["color4"]
color_fg = colors["special"]["foreground"]
color_noop = "#00000000"

c.colors.statusbar.normal.bg = color_bg
c.colors.statusbar.normal.fg = color_fg
c.colors.statusbar.progress.bg = color_fg
c.colors.statusbar.url.fg = color_fg
c.colors.statusbar.url.success.http.fg = color_fg
c.colors.statusbar.url.success.https.fg = color_fg
c.colors.tabs.bar.bg = color_noop
c.colors.tabs.even.bg = color_bg
c.colors.tabs.even.fg = color_fg
c.colors.tabs.odd.bg = color_bg
c.colors.tabs.odd.fg = color_fg
c.colors.tabs.selected.even.bg = color_bghl
c.colors.tabs.selected.even.fg = color_fghl
c.colors.tabs.selected.odd.bg = color_bghl
c.colors.tabs.selected.odd.fg = color_fghl
c.colors.webpage.darkmode.enabled = theme == "dark"
c.completion.cmd_history_max_items = 0
c.completion.open_categories = []
c.completion.web_history.max_items = 0
c.confirm_quit = ["downloads"]
c.content.blocking.enabled = False
c.content.cookies.accept = "no-3rdparty"
c.content.javascript.enabled = True
c.content.notifications.enabled = False
c.content.unknown_url_scheme_policy = "allow-all"
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

