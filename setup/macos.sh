# MacOS keyrepeat: ~167ms initial delay, ~62 repeat/s
defaults write -g InitialKeyRepeat -int 10
defaults write -g KeyRepeat -int 1
# Disable accent popup
defaults write -g ApplePressAndHoldEnabled -bool false
