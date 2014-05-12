import qualified Weechat

close :: Weechat.CloseCB
close dat buf = return Weechat.weechat_rc_ok

weechat_init :: IO Weechat.RC
weechat_init = do
    Weechat.register "example" "Shockk" "0.1" "GPL3" "Example Script" Nothing ""
    Weechat.print Weechat.nullPtr "Hello World!"
    Weechat.buffer_new "example" Nothing Weechat.nullPtr (Just close) Weechat.nullPtr
    return Weechat.weechat_rc_ok
