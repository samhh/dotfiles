import * as React from 'react';
import * as Oni from 'oni-api';

export const activate = (oni: Oni.Plugin.Api) => {
    console.log('config activated');
}

export const deactivate = (oni: Oni.Plugin.Api) => {
    console.log('config deactivated');
}

// Config!
export const configuration = {
    'oni.loadInitVim': true,
    'oni.useDefaultConfig': false,
    'statusbar.enabled': false,
    'sidebar.enabled': false,
    'tabs.mode': 'native',
    'ui.colorscheme': 'gruvbox_dark',
    'editor.fontSize': '16px',
    'editor.fontFamily': 'Fira Code',
};

