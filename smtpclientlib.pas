unit smtpclientlib;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fileutil,
  smtpsend, mimemess, mimepart, synautil;

{
  Based on Ararat Synapse library 0.40, by Lukas Gebauer (http://synapse.ararat.cz/)
  Ararat Synapse is a non-visual internet library
  This unit extends the Synapse TSmtpSend object to make it easier to use

  Sample usage:
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
        oSmtpClient.AddAttachment( ExtractFilePath( ParamStr( 0 ) ) + 'project1.lpi' );
        if oSmtpClient.Send( 'info@company.com', 'you@there.com', '', '', 'Buy more of our stuff' ) then
          ShowMessage( 'Send ok' )
        else
          ShowMessage( oSmtpClient.SendResult );
      finally
        oSmtpClient.Free;
      end;
    end;

    More docs and code:
      http://synapse.ararat.cz/doc/help/smtpsend.TSMTPSend.html
      http://stackoverflow.com/questions/15604243/how-to-send-a-e-mail-for-more-than-one-recipient-at-once-using-delphi-and-synap
      http://synapse.ararat.cz/doku.php/public:howto:sendingbcc
      http://synapse.ararat.cz/doku.php/public:howto:smtpsend = with TLS
      http://coding.derkeiler.com/Archive/Delphi/borland.public.delphi.thirdpartytools.general/2006-07/msg00249.html = attachments
      http://en.wikipedia.org/wiki/Simple_Mail_Transfer_Protocol
}

type

  { TSmtpClient }

  TSmtpClient = class( TSmtpSend )
  private
    // create parts also at object creation
    // could wait until actual Send(), but then no user customization would be possible for headers
    MessageParts: TMimeMess;
    Attachments: TStringList;
    FBodyAsHtml: TStringList;
    FBodyAsPlainText: TStringList;
    FSendResult: string;
    FServerAddress: string;
    FServerLoginPassword: string;
    FServerLoginUsername: string;
    FServerPort: string;
    procedure SetSendResult(AValue: string);
    procedure SetServerAddress(AValue: string);
    procedure SetServerLoginPassword(AValue: string);
    procedure SetServerLoginUsername(AValue: string);
    procedure SetServerPort(AValue: string);
    procedure SendResultClear;
    procedure SendResultAppend( sText: string );
  public
    // server related stuff - set once
    property ServerAddress: string read FServerAddress write SetServerAddress;
    property ServerPort: string read FServerPort write SetServerPort;
    property ServerLoginUsername: string read FServerLoginUsername write SetServerLoginUsername;
    Property ServerLoginPassword: string read FServerLoginPassword write SetServerLoginPassword;
    // messsage parts - set for each message
    // if there is both a plain text and html body, then normally all receiving software shows the html version only
    property BodyAsPlainText: TStringList read FBodyAsPlainText;
    property BodyAsHtml: TStringList read FBodyAsHtml;
    // save all replies from server
    property SendResult: string read FSendResult write SetSendResult;
    constructor Create;
    destructor Destroy; override;
    procedure MessageClear;
    function AddInlineAttachment( sFile: string; sContentID: string = ''; bReplaceInBodyHtml: Boolean = True ): Boolean;
    function AddAttachment( sFile: string ): Boolean;
    function Send( sFrom, sToList, sCCList, sBCCList, sSubject: string ): Boolean;
    {
      Other usefull methods of TSmtpSend:
        Self.SystemName
        Headers.CustomHeaders -> http://synapse.ararat.cz/doc/help/mimemess.TMessHeader.html#CustomHeaders
        Headers.Organization
        Headers.ReplyTo
    }
  end;

implementation

{ TSmtpClient }

procedure TSmtpClient.SetServerAddress(AValue: string);
begin
  if FServerAddress = AValue then
    Exit;
  FServerAddress := AValue;
  TargetHost := AValue;
end;

procedure TSmtpClient.SetSendResult(AValue: string);
begin
  if FSendResult = AValue then
    Exit;
  FSendResult := AValue;
end;

procedure TSmtpClient.SetServerLoginPassword(AValue: string);
begin
  if FServerLoginPassword = AValue then
    Exit;
  FServerLoginPassword := AValue;
  Password := AValue;
end;

procedure TSmtpClient.SetServerLoginUsername(AValue: string);
begin
  if FServerLoginUsername = AValue then
    Exit;
  FServerLoginUsername := AValue;
  UserName := AValue;
end;

procedure TSmtpClient.SetServerPort(AValue: string);
begin
  if FServerPort = AValue then
    Exit;
  FServerPort := AValue;
  TargetPort := AValue;
end;

procedure TSmtpClient.SendResultClear;
begin
  SendResult := '';
end;

procedure TSmtpClient.SendResultAppend(sText: string);
begin
  SendResult := SendResult + sText + #13#10;
end;

constructor TSmtpClient.Create;
begin
  inherited Create;
  MessageParts := TMimeMess.Create;
  Attachments := TStringList.Create;
  FBodyAsPlainText := TStringList.Create;
  FBodyAsHtml := TStringList.Create;
end;

destructor TSmtpClient.Destroy;
begin
  MessageParts.Free;
  Attachments.Free;
  FBodyAsPlainText.Free;
  FBodyAsHtml.Free;
  inherited Destroy;
end;

procedure TSmtpClient.MessageClear;
begin
  MessageParts.Clear;
  Attachments.Clear;
  BodyAsPlainText.Clear;
  BodyAsHtml.Clear;
end;

function TSmtpClient.AddInlineAttachment(sFile: string; sContentID: string = ''; bReplaceInBodyHtml: Boolean = True ): Boolean;
var
  s: string;
  i: Integer;
begin
  Result := FileExists( sFile );
  if Result then begin
    if sContentID = '' then
      sContentID := ExtractFileNameWithoutExt( ExtractFileName( sFile ) );
    if bReplaceInBodyHtml then begin
      s := LowerCase( ' src="' + sFile + '"' );
      i := Pos( s, LowerCase( BodyAsHtml.Text ) );
      if i = 0 then begin
        StringReplace( s, '"', '''', [ rfReplaceAll ] );
        i := Pos( s, LowerCase( BodyAsHtml.Text ) );
      end;
      if i > 0 then
        BodyAsHtml.Text := StringReplace( BodyAsHtml.Text, s, 'src="cid:' + sContentID + '"', [rfReplaceAll, rfIgnoreCase] );
    end;
    // save info; actual appending happens as Send()
    Attachments.Append( sContentID + '=' + sFile );
  end;
end;

function TSmtpClient.AddAttachment(sFile: string): Boolean;
begin
  Result := FileExists( sFile );
  // save info; actual appending happens as Send()
  if Result then
    Attachments.Append( sFile );
end;

function TSmtpClient.Send(sFrom, sToList, sCCList, sBCCList, sSubject: string): Boolean;
var
  sList, sAddress: string;
  MultiPartMix, MultiPartRel, MultiPartAlt: TMimePart;
  i: Integer;
begin
  Result := False;
  SendResultClear;
  // construct message headers
  MessageParts.Header.From := sFrom;
  // CommaText := sToList would be logical, but that also breaks at spaces
  sList := sToList;
  repeat
    sAddress := Trim( FetchEx( sList, ',', '"' ) );
    if ( sAddress <> '' ) then
      MessageParts.Header.ToList.Append( sAddress );
  until sList = '';
  sList := sCCList;
  repeat
    sAddress := Trim( FetchEx( sList, ',', '"' ) );
    if ( sAddress <> '' ) then
      MessageParts.Header.CCList.Append( sAddress );
  until sList = '';
  MessageParts.Header.Subject := sSubject;
  MessageParts.Header.Date := Now;
  MessageParts.Header.XMailer := 'Synapse ' + ExtractFileNameWithoutExt( ExtractFileNameOnly( ParamStr( 0 ) ) ); // defaults to synapse developer
  // add body and other parts
  // multiparts (parent-parts)
  MultiPartMix := MessageParts.AddPartMultipart( 'mixed', nil );
  MultiPartRel := MessageParts.AddPartMultipart( 'related', MultiPartMix );
  MultiPartAlt := MessageParts.AddPartMultipart( 'alternative', MultiPartRel );
  if BodyAsPlainText.Count > 0 then
    MessageParts.AddPartText( BodyAsPlainText, MultiPartAlt );
  if BodyAsHtml.Count > 0 then
    MessageParts.AddPartHTML( BodyAsHtml, MultiPartAlt );
  for i := 0 to Attachments.Count - 1 do begin
    if Attachments.Names[ i ] = '' then
      // no name means no name=value, so this is an unrelated file
      MessageParts.AddPartBinaryFromFile( Attachments[ i ],
                                          MultiPartMix )
    else
      // there is a name=value, so this is htmlbody-related file
      MessageParts.AddPartHTMLBinaryFromFile( Attachments.ValueFromIndex[ i ],
                                              Attachments.Names[ i ],
                                              MultiPartRel );
  end;
  // message is ready
  MessageParts.EncodeMessage;
  if Login then begin
    SendResultAppend( 'Login: ' + FullResult.Text );
    // inform server who is sending: send 'MAIL FROM:' command - use only the emailaddress (skip name)
    MailFrom( GetEmailAddr( sFrom ), Length( MessageParts.Lines.Text ) );
    SendResultAppend( 'Mail from ' + GetEmailAddr( sFrom ) + ': ' + FullResult.Text );
    // inform server where this has to go: send 'RCPT TO:' command (more receivers, means more of this command)
    sList := sToList + ',' + sCCList + ',' + sBCCList;
    repeat
      sAddress := GetEmailAddr( Trim( FetchEx( sList, ',', '"') ) );
      if ( sAddress <> '' ) then begin
        Result := MailTo( sAddress );
        SendResultAppend( 'Mail to ' + sAddress + ': ' + FullResult.Text );
      end;
    until sList = '';
    // send the message
    if MailData( MessageParts.Lines ) then
      Result := True;
    SendResultAppend( 'Send: ' + FullResult.Text );
    Logout;
    SendResultAppend( 'Logout: ' + FullResult.Text );
  end
  else
    SendResultAppend( 'Login: ' + FullResult.Text );
end;

end.

