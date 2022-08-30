{ config, pkgs, ... }:

{
  home-manager.users.${config.username}.programs.aerc = {
    enable = true;

    extraAccounts = {
      "@samhh.com" = {
        source = "imaps://hello%40samhh.com@imap.migadu.com";
        source-cred-cmd = "pass show ${config.email.pass.path}";
        outgoing = "smtps://hello%40samhh.com@smtp.migadu.com";
        outgoing-cred-cmd = "pass show ${config.email.pass.path}";
        default = "INBOX";
        from = "${config.fullName} <${config.email.address}>";
        copy = "Sent";
      };

      "@unsplash.com" = {
        source = "imaps://sam%40unsplash.com@imap.gmail.com";
        source-cred-cmd = "pass show emails/unsplash.com/sam";
        outgoing = "smtps+plain://sam%40unsplash.com@smtp.gmail.com";
        outgoing-cred-cmd = "pass show emails/unsplash.com/sam";
        default = "INBOX";
        from = "${config.fullName} <sam@unsplash.com>";
        copy = "[Gmail]/Sent Mail";
      };
    };

    extraConfig = {
      general.unsafe-accounts-conf = true;

      ui = {
        sidebar-width = 15;
        index-format = "%D %-20.20n %Z %s";
        timestamp-format = "2006-01-02 15:04";
      };

      viewer = {
        alternatives = "text/plain,text/html";
        header-layout = "From|To,Cc|Bcc,Date,Subject";
        always-show-mime = true;
      };

      compose = {
        header-layout = "To|From,Subject";
        address-book-cmd = "${pkgs.khard}/bin/khard email --parsable --remove-first-line '%s'";
      };

      filters =
        let
          filter = x: "${pkgs.aerc}/share/aerc/filters/${x}";
          awk = "${pkgs.gawk}/bin/awk";
        in
        {
          "subject,~^\\[PATCH" = "${awk} -f ${filter "hldiff"}";
          "text/html" = filter "html";
          "text/*" = "${awk} -f ${filter "plaintext"}";
          "image/*" = "${pkgs.imv}/bin/imv -";
          "application/pdf" = "${pkgs.zathura}/bin/zathura -";
        };

      triggers.new-email = "exec ${pkgs.libnotify}/bin/notify-send \"Email from %n\" \"%s\"";
    };

    extraBinds = {
      "global" = {
        "gT" = ":prev-tab<Enter>";
        "gt" = ":next-tab<Enter>";
      };

      "messages" = {
        "<Down>" = ":next<Enter>";
        "<C-d>" = ":next 50%<Enter>";
        "<Up>" = ":prev<Enter>";
        "<C-u>" = ":prev 50%<Enter>";
        "gg" = ":select 0<Enter>";
        "G" = ":select -1<Enter>";
        "gf" = ":next-folder<Enter>";
        "gF" = ":prev-folder<Enter>";
        "<Enter>" = ":view<Enter>";
        "S" = ":flag -t -x Flagged<Enter>";
        "V" = ":read -t<Enter>";
        "A" = ":move Archive<Enter>";
        "gA" = ":move [Gmail]/All Mail<Enter>";
        "W" = ":move Awaiting<Enter>";
        "U" = ":move Unfulfilled<Enter>";
        "D" = ":move Trash<Enter>";
        "gD" = ":move [Gmail]/Trash<Enter>";
        "C" = ":compose<Enter>";
        "ff" = ":forward<Enter>";
        "rr" = ":reply<Enter>";
        "rq" = ":reply -q<Enter>";
        "Rr" = ":reply -a<Enter>";
        "Rq" = ":reply -aq<Enter>";
        "/" = ":search<space>";
        "\\" = ":filter<space>";
        "n" = ":next-result<Enter>";
        "N" = ":prev-result<Enter>";
      };

      "view" = {
        "q" = ":close<Enter>";
        "o" = ":open<Enter>";
        "ff" = ":forward<Enter>";
        "rr" = ":reply<Enter>";
        "rq" = ":reply -q<Enter>";
        "Rr" = ":reply -a<Enter>";
        "Rq" = ":reply -aq<Enter>";
        "gP" = ":prev-part<Enter>";
        "gp" = ":next-part<Enter>";
        "go" = ":pipe ${pkgs.urlscan}/bin/urlscan<Enter>";
      };

      "compose" = {
        "$ex" = "<C-w>";
        "<C-u>" = ":prev-field<Enter>";
        "<C-d>" = ":next-field<Enter>";
      };

      "compose::editor" = {
        "$noinherit" = "true";
        "$ex" = "<C-w>";
        "<C-u>" = ":prev-field<Enter>";
        "<C-d>" = ":next-field<Enter>";
      };

      "compose::review" = {
        "y" = ":send<Enter>";
        "n" = ":abort<Enter>";
        "p" = ":postpone<Enter>";
        "q" = ":abort<Enter>";
        "e" = ":edit<Enter>";
        "a" = ":attach<space>";
      };
    };
  };
}
