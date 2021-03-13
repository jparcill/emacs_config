Xephyr -br -ac -noreset -screen 1915x1080 :1 &
sleep 0.2
DISPLAY=:1 emacs
