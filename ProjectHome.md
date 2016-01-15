An implementation of a (non-visual) email client, based on the great Synapse internet library. Code is for Lazarus 1.0.x / Freepascal.

It supports most (all?) features that one would usually want in sending emails:
  * Send to one or more receivers
  * CC to one or more receivers
  * BCC to one or more receivers
  * Plain text body
  * Html text body
  * Add inline image(s), shown in the html body
  * Add attachment(s)

> Sample usage:
```
    procedure TForm1.ButtonClick(Sender: TObject);
    var
      oSmtpClient: TSmtpClient;
    begin
      oSmtpClient := TSmtpClient.Create;
      try
        oSmtpClient.ServerDomain := 'smtp.mandrillapp.com';
        oSmtpClient.ServerPort := '587';
        oSmtpClient.ServerLoginUsername := 'me@here.com';
        oSmtpClient.ServerLoginPassword := 'difficult-and-long-password';
        oSmtpClient.MessageClear;
        oSmtpClient.BodyAsPlainText.Text := 'Plain body text.';
        oSmtpClient.BodyAsHtml.Text := '<html><head></head><body><h2>Html body text.</h2><img src="C:\lazarus\examples\jpeg\lazarus.jpg" /></body></html>';
        oSmtpClient.AddInlineAttachment( 'C:\lazarus\examples\jpeg\lazarus.jpg' );
        oSmtpClient.AddAttachment( ExtractFilePath( ParamStr( 0 ) ) + 'project.doc' );
        if oSmtpClient.Send( 'info@company.com', 'you@there.com', '', '', 'Buy more of our stuff' ) then
          ShowMessage( 'Send ok' )
        else
          ShowMessage( oSmtpClient.SendResult );
      finally
        oSmtpClient.Free;
      end;
    end;
```