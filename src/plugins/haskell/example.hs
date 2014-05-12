import qualified Weechat

close :: Weechat.CloseCB
close _ buffer = do
    Weechat.print Weechat.nullPtr "buffer closed"
    return Weechat.weechat_rc_ok

weechat_init :: IO Weechat.RC
weechat_init = do
    Weechat.register "example" "Shockk" "0.1" "GPL3" "Example Script" Nothing ""
    buf <- Weechat.buffer_new "example" Nothing Weechat.nullPtr (Just close) Weechat.nullPtr
    Weechat.print buf "Hello World!"
    return Weechat.weechat_rc_ok
