#!/usr/bin/env bash
xmodmap -e "clear mod4"
xmodmap -e "clear control"
xmodmap -e "clear shift"
xmodmap -e "clear space"
xmodmap -e "keycode 110 = Control_L"
xmodmap -e "keycode 119 = Return"
xmodmap -e "keycode 115 = Down Up"
xmodmap -e "keycode 65 = Shift_L"
xmodmap -e "keycode 36 = space"
xmodmap -e "keycode 49 = Super_L"
xmodmap -e "keycode 113 = Super_L"
xmodmap -e "keycode 133 = Hyper_R"
xmodmap -e "keycode 23 = Super_R"
xmodmap -e "keycode 173 = Tab"
xmodmap -e "keycode 172 = Escape"
xmodmap -e "keycode 9 = grave asciitilde"
xmodmap -e "add mod4 = Super_L Super_R Hyper_L"
xmodmap -e "add control = Control_L Control_R"
xmodmap -e "add shift = Shift_L Shift_R"
xcape -e 'Control_L=Escape;Control_R=Escape;Super_R=Tab'

syncthing -no-browser
