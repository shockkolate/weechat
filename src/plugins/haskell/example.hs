weechat_init :: IO Weechat.RC
weechat_init = do
    Weechat.register "example" "Shockk" "0.1" "GPL3" "Example Script" "" ""
    return Weechat.weechat_rc_ok
