import qualified Weechat

start :: IO Weechat.RC
start = do
    Weechat.plugin_get_name "" >>= Weechat.print ""
    buf <- Weechat.buffer_new "example" Nothing "" (Just close) ""
    Weechat.print buf "Hello World!"
    return Weechat.weechat_rc_ok

close :: Weechat.CloseCB
close _ _ = do
    Weechat.print "" "buffer closed"
    return Weechat.weechat_rc_ok

weechat_init :: IO Weechat.RC
weechat_init = do
    reg <- Weechat.register "example" "Shockk" "0.1" "GPL3" "Example Script" Nothing ""
    if reg == 1 then start else return Weechat.weechat_rc_error
