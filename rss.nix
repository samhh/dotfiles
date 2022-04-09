{ pkgs, ... }:

let
  uname = "sam";
in {
  home-manager.users.${uname} = {
    programs.newsboat = {
      enable = true;
      browser = "\"${pkgs.qutebrowser}/bin/qutebrowser %u &\"";
      extraConfig = ''
        confirm-exit yes
        text-width 80

        bind-key j down
        bind-key k up
        bind-key g home
        bind-key G end
        bind-key ^U pageup
        bind-key ^D pagedown
        bind-key s sort
        bind-key r reload
        bind-key R reload-all
        bind-key A mark-feed-read
        bind-key o open-in-browser-noninteractively
        bind-key u show-urls
        bind-key ESC quit
        bind-key Q hard-quit
      '';
      urls = [
        { url = "https://firevlondon.com/feed/"; title = "~FIRE v London"; tags = [ "fire" ]; }
        { url = "https://simplelivingsomerset.wordpress.com/feed/"; title = "~Simple Living In Somerset"; tags = [ "fire" ]; }

        { url = "https://www.rollingstone.com/author/matt-taibbi/feed/"; title = "~Rolling Stone - Matt Taibbi"; tags = [ "politics" ]; }
        { url = "https://www.journalofcontroversialideas.org/rss"; title = "~Journal of Controversial Ideas"; tags = [ "politics" ]; }

        { url = "https://samhh.com/rss.xml"; title = "~samhh"; tags = [ "dev" ]; }
        { url = "https://increment.com/feed.xml"; title = "~Increment"; tags = [ "dev" ]; }
        { url = "https://alistapart.com/main/feed/"; title = "~A List Apart"; tags = [ "dev" ]; }
        { url = "https://lexi-lambda.github.io/feeds/all.rss.xml"; title = "~Alexis King"; tags = [ "dev" ]; }
        { url = "https://codersteve.dev/post/index.xml"; title = "~Coder Steve"; tags = [ "dev" ]; }
        { url = "https://nasamuffin.github.io/feed.xml"; title = "~Emily Shaffer"; tags = [ "dev" ]; }
        { url = "https://juliu.is/rss.xml"; title = "~Ju Liu"; tags = [ "dev" ]; }
        { url = "https://www.michaelpj.com/blog/feed.xml"; title = "~Michael Peyton Jones"; tags = [ "dev" ]; }
        { url = "https://www.snoyman.com/rss.xml"; title = "~Michael Snoyman"; tags = [ "dev" ]; }
        { url = "https://blog.drewolson.org/feed.xml"; title = "~Drew Olson"; tags = [ "dev" ]; }
        { url = "https://benhoyt.com/writings/rss.xml"; title = "~Ben Hoyt"; tags = [ "dev" ]; }
        { url = "https://chrispenner.ca/atom.xml"; title = "~Chris Penner"; tags = [ "dev" ]; }
        { url = "https://www.haskellforall.com/feeds/posts/default?alt=rss"; title = "~Haskell for all"; tags = [ "dev" ]; }
        { url = "https://neilmitchell.blogspot.com/feeds/posts/default?alt=rss"; title = "~Neil Mitchell"; tags = [ "dev" ]; }
        { url = "https://jelv.is/blog/atom.xml"; title = "~Tikhon Jelvis"; tags = [ "dev" ]; }
        { url = "https://www.stephendiehl.com/feed.atom"; title = "~Stephen Diehl"; tags = [ "dev" ]; }
        { url = "https://www.parsonsmatt.org/feed.xml"; title = "~Matt Parsons"; tags = [ "dev" ]; }
        { url = "https://williamyaoh.com/feed.atom"; title = "~William Yao"; tags = [ "dev" ]; }
        { url = "https://luctielen.com/atom.xml"; title = "~Luc Tielen"; tags = [ "dev" ]; }
        { url = "https://blog.poisson.chat/rss.xml"; title = "~Li-yao Xia"; tags = [ "dev" ]; }
        { url = "https://chrisdone.com/rss.xml"; title = "~Chris Done"; tags = [ "dev" ]; }
        { url = "https://blog.ploeh.dk/rss.xml"; title = "~ploeh"; tags = [ "dev" ]; }
        { url = "https://phaazon.net/blog/feed"; title = "~Dimitri Sabadie"; tags = [ "dev" ]; }

        { url = "https://www.linuxjournal.com/node/feed"; title = "~Linux Journal"; tags = [ "tech" ]; }
        { url = "https://archlinux.org/feeds/news/"; title = "~Arch Linux"; tags = [ "tech" ]; }
        { url = "https://drewdevault.com/blog/index.xml"; title = "~Drew DeVault"; tags = [ "tech" ]; }
        { url = "https://alexschroeder.ch/wiki/feed/full/"; title = "~Alex Schroeder"; tags = [ "tech" ]; }
        { url = "http://neovim.io/news.xml"; title = "~Neovim"; tags = [ "tech" ]; }
        { url = "https://christine.website/blog.rss"; title = "~Christine Dodrill"; tags = [ "tech" ]; }

        { url = "https://boilingsteam.com/feed/"; title = "~Boiling Steam"; tags = [ "gaming" ]; }
      ];
    };
  };
}
