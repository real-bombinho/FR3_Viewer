unit DPControls;

//////////////////////////////////////////////////////////////////////////////
//
// DPControls,  2018 bombinho
//
// Version 0.2
//
//////////////////////////////////////////////////////////////////////////////

interface
uses System.Types, System.UITypes, System.Classes, System.Generics.Collections,
     System.SysUtils, FMX.Graphics, FMX.Types, FMX.Objects, FMX.Dialogs,
     DPFile, DPColors, DPError, XML.XMLDoc, XML.XMLintf;

const

  frxFactor = 3.77953;

type

TControlType = (TextBox, Image, Shape, Line);

TShapeType = (skRectangle, skRoundRectangle, skEllipse, skTriangle);

TDPPage = class;

TLine = record
  Show: boolean;
  Color: TAlphaColor;
  Style: TBrushKind;
  Width: single;
end;

TControlFrame = class
private
  FWidth: single;
  FColor: TAlphaColor;
  procedure setTyp(const Value: integer);
  function getTyp: integer;
  procedure setWidth(const Value: single);
public
  Line: array[TSide] of TLine;
  Typ: TSides;
  property Color: TAlphaColor read FColor write FColor;
  property Width: single read FWidth write setWidth;
  property Value: integer read getTyp write setTyp;
  function PenWidth: single;
  function IsFrameEntry(const Name: string; const Value: string): boolean;
  procedure Draw(Canvas: TCanvas; const ClipRect: TRectF; const Opacity: single = 1);
  procedure DrawGuide(Canvas: TCanvas; const ClipRect: TRectF; const Opacity: single = 1);
  constructor Create;
end;

TDPFont = class
  private
    FStyles: TFontStyles;
    FCharSet: Byte;
    FColor: TAlphaColor;
    FHeight: Single;
    FName: TFontName;
    FWidth: single;
    procedure SetWidth(const Value: single);
    procedure setFontStyles(value: string);
    function getHeight: Single;
    procedure setHeight(const Value: single);
  public
    VFactor: single;
    property CharSet: Byte read FCharset write FCharset;
    property Color: TAlphaColor read FColor write FColor;
    property Height: single read getHeight write setHeight;
    property Width: single read FWidth write SetWidth;
    property Name: TFontName read FName write FName;
    property Styles: TFontStyles read FStyles write FStyles;
    function IsFontEntry(const Name: string; const Value: string): boolean;

    constructor Create;
    //destructor Destroy;
  end;

TDPControlCustom = class(TObject)
  private
    FItemType: TControltype;
    FWidth: single;
    FHeight: single;
    FPage: TDPPage;
    FShowHint: boolean;
    FRestrictions: integer;
    function Pos(PosInMm: TPosition): TPosition;
    procedure SetShowHint(const Value: boolean);
    function getTop: single;
    procedure setTop(const Value: single);
    function getLeft: single;
    procedure setLeft(const Value: single);
    procedure setHeight(const Value: single);
    function getHeight: single;
    function getWidth: single;
    procedure setWidth(const Value: single);
    function getColor: TAlphacolor;
    procedure setColor(const Value: TAlphacolor);
  protected
    FFrame: TControlFrame;
    FOpacity: single;
    FName: String;
    FStyle: TBrushKind;
    function PenWidth: single;
    function HFactor: single;
    function VFactor: single;
    function getClipRect: TRectF;
    function IsControlEntry(const Name: string; const Value: string): boolean;
    property ClipRect: TRectF read getClipRect;
  public
    Position: TPosition;
    property Color: TAlphacolor read getColor write setColor;
    property Frame: TControlFrame read FFrame;
    property Top: single read getTop write setTop;
    property Left: single read getLeft write setLeft;
    property Name: String read FName;
    property Opacity: single read FOpacity write FOpacity;
    property Width: single read getWidth write setWidth;
    property Height: single read getHeight write SetHeight;
    property ShowHint: boolean read FShowHint write SetShowHint;
    procedure Draw(Canvas: TCanvas;const Zoom: single); virtual;
    constructor Create;
  end;

//TDPMemoView = class;

TDPTextBox = class(TDPControlCustom)
  private
    FText: String;
    FMemo: TStringList;
    FHAlign: TTextAlign;
    FVAlign: TTextAlign;
    FParentFont: boolean;
    FBoxColor: TAlphaColor;
    procedure setBackGroundColor(const Color: TAlphaColor);
    procedure setText(S: String);
    procedure SetParentFont(const Value: boolean);
    function getHAlign(Value: String): TTextAlign;
    function getVAlign(Value: String): TTextAlign;
    function getColor: TAlphacolor;
    procedure setColor(const Value: TAlphacolor);
  public
    Font: TDPFont;
    property Color: TAlphacolor read getColor write setColor;
    property ParentFont: boolean read FParentFont write SetParentFont;
    property Text: string read FText write setText;
    property BoxColor: TAlphaColor read FBoxColor write setBackGroundColor;
    procedure Draw(Canvas: TCanvas; const Zoom: single = 1);
    constructor Create(const Page: TDPPage);
    constructor CreateFromNode(Node: IXMLNode; const Page: TDPPage);
    destructor Destroy;
  end;

TDPLine = class(TDPControlCustom)
  private
    FDiagonal: boolean;
  public
    procedure Draw(Canvas: TCanvas; const Zoom: single = 1);
    constructor Create(Page: TDPPage);
    constructor CreateFromNode(Node: IXMLNode; const Page: TDPPage);
  end;

TDPShape = class(TDPControlCustom)
  private
    FCorners : TCorners;
    FCornerType: TCornerType;
    FCornerRadius: single;
    FShape: TShapeType;
    procedure SetCornerRadius(const Value: single);
    procedure SetShape(const Value: String);
    procedure setShapeValue(const Value: TShapeType);
    function getCornerRadius: single;
    function getLineWidth: single;
    procedure setLineWidth(const Value: single);
  public
    property Shape: TShapeType read FShape write setShapeValue;
    property LineWidth: single read getLineWidth write setLineWidth;
    property CornerType: TCornerType read FCornerType write FCornerType;
    property CornerRadius: single read getCornerRadius write SetCornerRadius;
    procedure Draw(Canvas: TCanvas; const Zoom: single = 1);
    constructor Create(Page: TDPPage);
    constructor CreateFromNode(Node: IXMLNode; const Page: TDPPage);
  end;

TDPImage = class(TDPControlCustom)
  private
    Fbmp: TBitmap;
    FHighQuality: boolean;
    FTransparent: boolean;
    FAutoSize: boolean;
    FTransparentColor: TAlphaColor;
    procedure setTransparent(const Value: boolean);
    procedure setBmp(const Value: TBitmap);
  public
    property Transparent: boolean read Ftransparent write setTransparent;
    property BitMap: TBitmap read Fbmp write setBmp;
    procedure Draw(Canvas: TCanvas; const Zoom: single = 1);
    constructor Create(Page: TDPPage);
    constructor CreateFromNode(Node: IXMLNode; const Page: TDPPage);
  end;

TDPPage = class
// A4 @ 300dpi = 2512 x 3488
// A5 @ 300dpi = 1256 x 1744 ...
strict private

    FFont: TDPFont;
    FHeight: single;
    FWidth: single;
    FOrientation: TScreenOrientation;
    FBackground: TAlphaColor;
    FColor: TAlphaColor;
    FPaintBox: TPaintBox;
    FItems: Array of TDPControlCustom;
    FPaperSize: integer;
private
  const a = 2512;
        b = 3488;

    procedure SetPaperSize(const Value: integer);
    function getBottomMargin: single;
    function getLeftMargin: single;
    function getRightMargin: single;
    function getTopMargin: single;
  var
    FTopMargin: single;
    FLeftMargin: single;
    FRightMargin: single;
    FBottomMargin: single;
    ColumnWidth: single;
    ColumnPositions: string;
    HGuides: string;
    VGuides: string;
    FCount: Array[TControlType] of integer;
    procedure SetLeftMargin(const Value: single);
    procedure SetRightMargin(const Value: single);
    procedure SetTopMargin(const Value: single);
    procedure SetBottomMargin(const Value: single);
public
  PaperWidth: single; // 210.000000000000000000;
  PaperHeight: single; // 297.000000000000000000;
  Name: string;
  property PaperSize: integer read FPaperSize write SetPaperSize; // A4 = 9;
  property Height: single read FHeight;
  property Width: single read FWidth;
  property LeftMargin: single read getLeftMargin write SetLeftMargin;
  property RightMargin: single read getRightMargin write SetRightMargin;
  property TopMargin: single read getTopMargin write SetTopMargin;
  property BottomMargin: single read getBottomMargin write SetBottomMargin;
  property Font: TDPFont read FFont;
  property Orientation: TScreenOrientation read FOrientation;
  property BackGroundColor: TAlphaColor read FBackGround;
  procedure AddItem(Item: TDPControlCustom);
  procedure Draw(Canvas: TCanvas; const Zoom: single = 1);

  constructor Create(AOwner: TComponent);
  constructor CreateFromNode(Node: IXMLNode);
  destructor Destroy;
end;

TDPDataPage = class
  FName: String;
  FTop, FLeft, FHeight, FWidth: integer;
  constructor Create(const Name: string; Top, Left, Height, Width: integer);
  constructor CreateFromNode(Node: IXMLNode);
end;

TPreviewOptions = record
  Buttons: integer;
  Zoom: single;
end;

TPrintOptions = record
  Printer: string;
  PrintOnSheet: integer;
  PageNumbers: integer;
end;

TReportOptions = record
  CreateDate: TDateTime;
  Description: string;
  LastChange: TDateTime;
  roName: String;
  Author: String;
  VersionMajor: integer;
  VersionMinor: integer;
  VersionRelease: integer;
  function IsEntry(const Name: string; const Value: string): boolean;
end;


TDPReport = class
private
  FPages: Array of TDPPage;
    function getPage(Index: integer): TDPPage;
public
  Version: string;
  DotMatrixReport: boolean;
  IniFile: string;
  PreviewOptions: TPreviewOptions;
  PrintOptions: TPrintOptions;
  ReportOptions: TReportOptions;
  DataPage: TDPDataPage;
  Pages_: TObjectList<TDPPage>;
  property Pages[Index: integer]: TDPPage read getPage;
  procedure addPage(const value: TDPPage);
  function LoadFromFR3(const filename: TFileName): boolean;
  constructor Create;
end;

implementation

const LineCorrection = 2;

{  TDPControlCustom }

function TDPControlCustom.getClipRect: TRectF;
var x, y: single;
    x1, y1: single;
begin
  x := Position.X + FPage.FLeftMargin;
  y := Position.Y + FPage.FTopMargin;
  x1 := x + FWidth;
  if x1 > FPage.Width then x1 := FPage.Width;

  Result.Create(x, y, X1, Y + FHeight);
end;

function TDPControlCustom.getColor: TAlphacolor;
begin
  result := FFrame.FColor;
end;

function TDPControlCustom.getHeight: single;
begin
  result := FHeight / VFactor;
end;

function TDPControlCustom.getLeft: single;
begin
  result := Position.X / HFactor;
end;

function TDPControlCustom.getTop: single;
begin
  result := Position.Y / VFactor;
end;

function TDPControlCustom.getWidth: single;
begin
  result := FWidth / HFactor;
end;

function TDPControlCustom.HFactor: single;
begin
  if FPage <> nil then
    result := FPage.Width / FPage.PaperWidth
  else
    result := 1;
end;

function TDPControlCustom.IsControlEntry(const Name, Value: string): boolean;
begin
  result := true;
  if Name = 'NAME' then FName := Value
  else
  if Name = 'WIDTH' then Width := StrToFloatDef(Value, 0) / FrxFactor
  else
  if Name = 'HEIGHT' then Height := StrToFloatDef(Value, 0) / FrxFactor
  else
  if Name = 'TOP' then setTop(StrToFloatDef(Value, 0) / FrxFactor)
  else
  if Name = 'LEFT' then setLeft(StrToFloatDef(Value, 0) / FrxFactor)
  else
  if Name = 'SHOWHINT' then FShowHint := StrToBoolDef(Value, False)
  else
  if Name = 'RESTRICTIONS' then FRestrictions := StrToIntDef(Value, 0)
  else
    result := false;
end;

function TDPControlCustom.PenWidth: single;
begin
;
end;

function TDPControlCustom.Pos(PosInMm: TPosition): TPosition;
var rh, rw: single;
begin
  rh := TDPPage.a / 210;
  rw := TDPPage.b / 297;
  result := TPosition.Create(PointF(PosInMm.X, PosInMm.Y));
end;

procedure TDPControlCustom.setColor(const Value: TAlphacolor);
begin
  FFrame.FColor := value;
end;

procedure TDPControlCustom.SetHeight(const Value: single);
begin
  FHeight := Value * VFactor;
end;

procedure TDPControlCustom.SetLeft(const Value: single);
begin
  Position.X := Value * HFactor;
end;

procedure TDPControlCustom.SetShowHint(const Value: boolean);
begin
  FShowHint := Value;
end;

procedure TDPControlCustom.setTop(const Value: single);
begin
  Position.Y := Value * VFactor;
end;

procedure TDPControlCustom.setWidth(const Value: single);
begin
  FWidth := Value * HFactor;
end;

function TDPControlCustom.VFactor: single;
begin
  if FPage <> nil then
    result := FPage.Height / FPage.PaperHeight
  else result := 1;
end;

procedure TDPControlCustom.Draw(Canvas: TCanvas; const Zoom: single);
begin

end;

constructor TDPControlCustom.Create;
begin
  FFrame := TControlFrame.Create;
  Position := TPosition.Create(TPointF.Create(0,0));
  FStyle := TBrushKind.Solid;
  FOpacity := 1;
  FShowHInt := false;
end;

{ TDPTextBox }

constructor TDPTextBox.Create(const Page: TDPPage);
begin
  inherited Create;
  FFrame := TControlFrame.Create;
  Font := TDPFont.Create;
  FParentFont := true;
  FItemType := TControltype.TextBox;
  Page.AddItem(TDPControlCustom(Self));
  Font.VFactor := Page.Font.VFactor;
  Height := 22;
  Width := 100;
  FHalign := TTextAlign.Leading;
  FVAlign := TTextAlign.Center;
  setBackGroundColor(0);//TAlphaColors.Red);
  FName := 'TextBox' + inttostr(Page.FCount[FItemType]);
end;

constructor TDPTextBox.CreateFromNode(Node: IXMLNode; const Page: TDPPage);
var i: integer;
    s, t: string;
begin
  Create(Page);
  for I := 0 to Node.AttributeNodes.Count - 1 do
  begin
    s := uppercase(Node.AttributeNodes[i].NodeName);
    t := Node.AttributeNodes[i].Text;
    if Font.IsFontEntry(s, t) then
    else
    if IsControlEntry(s, t) then
    else
    if FFrame.IsFrameEntry(s, t) then
    else
    if s = 'PARENTFONT' then
      SetParentFont(StrToBoolDef(t, True))
    else
    if s = 'TEXT' then
      FText := t
    else
    if s = 'HALIGN' then
      FHAlign := getHAlign(t)
    else
    if s = 'VALIGN' then
      FVAlign := getVAlign(t)
    else
    if s = 'COLOR' then
      BoxColor := ColorToAlphaColor(t)
    else
      Error.Show('TFrxMemoView', Node.AttributeNodes[i].NodeName + ' - ' +
        Node.AttributeNodes[i].Text);
  end;
end;

procedure TDPTextBox.setBackGroundColor(const Color: TAlphaColor);
begin
  FBoxColor := Color;
end;

procedure TDPTextBox.setColor(const Value: TAlphacolor);
begin
  Font.Color := Value;
end;

procedure TDPTextBox.SetParentFont(const Value: boolean);
begin
  FParentFont := Value;
end;

procedure TDPTextBox.setText(S: string);
begin
  FText := S;
end;

procedure TDPTextBox.Draw(Canvas: TCanvas; const Zoom: single = 1);
var r,p: TRectF;

begin
  r := ClipRect;
  if BoxColor <> 0 then
  begin
    Canvas.Fill.Color := FBoxColor;
    Canvas.FillRect(r, 0, 0, AllCorners, 100);
  end;
  if FParentFont then
  begin
    Canvas.Font.Size := FPage.Font.FHeight * Font.VFactor;
    Canvas.Font.Family := FPage.Font.FName;
    Canvas.Font.Style := FPage.Font.FStyles;
  end
  else
  begin
    Canvas.Font.Size := Font.FHeight * Font.VFactor;
    Canvas.Font.Family := Font.FName;
    Canvas.Font.Style := Font.FStyles;
  end;
  Canvas.Fill.Color := Font.Color;
  Canvas.FillText(r, FText, false, FOpacity, [{TFillTextFlag.ftRightToLeft}], FHAlign, FValign);
  FFrame.Draw(Canvas, r, FOpacity);
  FFrame.DrawGuide(Canvas, r);
  //Canvas.FillText(r, FText, false, 1, [{TFillTextFlag.ftRightToLeft}], TTextAlign.taCenter, TTextAlign.taCenter);
end;

function TDPTextBox.getColor: TAlphacolor;
begin
  result := Font.Color;
end;

function TDPTextBox.getHAlign(Value: String): TTextAlign;
var s: string;
begin
  s := uppercase(value);
  result := TTextAlign.Leading;
  if s = 'HACENTER' then result := TTextAlign.Center
  else
  if s = 'HARIGHT' then result := TTextAlign.Trailing
  else
  if s = 'HABLOCK' then result := TTextAlign.Center;
end;

function TDPTextBox.getVAlign(Value: String): TTextAlign;
var s: string;
begin
  s := uppercase(value);
  result := TTextAlign.Center;
  if s = 'VATOP' then result := TTextAlign.Leading
  else
  if s = 'VABOTTOM' then result := TTextAlign.Trailing;
end;

destructor TDPTextBox.Destroy;
begin
  Font.Free;
end;

{ TDPImage }

constructor TDPImage.Create(Page: TDPPage);
begin
  inherited Create;
  FItemType := TControltype.Image;
  Page.AddItem(TDPControlCustom(Self));
  Height := 22;
  Width := 100;
  FName := 'Image' + inttostr(Page.FCount[FItemType]);
end;

constructor TDPImage.CreateFromNode(Node: IXMLNode; const Page: TDPPage);
var i: integer;
    s, t: string;
    st: RawByteString;

  procedure loadPicture(const value: string);
  var i: integer;
      str: string;
      strm: TMemoryStream;
  begin
    i := 41;
    st := '';
    while i < length(value) do
    begin
      str := '$' + copy(value, i, 2);
      st := st + ansiChar(strtointdef(str, 0));
      inc(i, 2);
    end;
    strm := TMemoryStream.Create;
    strm.write(st[1], length(st));
    strm.Position := 0;
    if Fbmp = nil then
      FBmp := TBitmap.Create;
    Fbmp.LoadFromStream(strm);
    strm.Free;
  end;

begin
  inherited Create;
  FItemType := TControltype.Image;
  Page.AddItem(TDPControlCustom(Self));
  for I := 0 to Node.AttributeNodes.Count - 1 do
  begin
    s := uppercase(Node.AttributeNodes[i].NodeName);
    t := Node.AttributeNodes[i].Text;
    if IsControlEntry(s, t) then
    else
    if s = 'HIGHTQUALITY' then
      FHighQuality := strToBoolDef(t, true)
    else
    if s = 'TRANSPARENT' then
      FTransparent := strToBoolDef(t, true)
    else
    if s = 'TRANSPARENTCOLOR' then
      FTransparentColor := ColorToAlphaColor(t)
    else
    if s = 'AUTOSIZE' then
      FAutoSize := strToBoolDef(t, true)
    else
    if s = 'PICTURE.PROPDATA' then
      loadPicture(t)
    else
      Error.Show('TFrxPictureView', Node.AttributeNodes[i].NodeName + ' - ' + t);
  end;
end;

procedure TDPImage.Draw(Canvas: TCanvas; const Zoom: single = 1);
var r: TRectF;
    temp: TBitMapData;
    TargetCol: TAlphaColor;
    tempPix: TAlphaColor;
    i, j: integer;
begin
  if assigned(Fbmp) then
  begin
    r := Fbmp.BoundsF;
    Canvas.DrawBitmap(Fbmp, r, ClipRect, FOpacity, false);
  end;
end;

procedure TDPImage.setBmp(const Value: TBitmap);
begin
  Fbmp := Value;
  setTransparent(FTransparent);
end;

procedure TDPImage.setTransparent(const Value: boolean);
var temp: TBitMapData;
    tempPix: TAlphaColor;
    i, j: integer;
begin
  Ftransparent := Value;
  if Fbmp <> nil then
  begin
    Fbmp.Map(TMapAccess.ReadWrite, temp);
    if value then
    begin
      if FTransparentColor = 0 then
        FTransparentColor := temp.GetPixel(0, 0);
    end;
    for i :=0 to  temp.Width do
    begin
      for j := 0 to temp.Height do
      begin
        tempPix := temp.GetPixel(i, j);
        if Ftransparent then
        begin
          if (tempPix = FTransparentColor) then
            temp.SetPixel (i, j, TAlphaColors.Null);
        end
        else
        begin
          if (tempPix = TAlphaColors.Null) then
            temp.SetPixel (i, j, FTransparentColor);
        end;
      end;
    end;
    FBmp.Unmap(temp);
  end;
end;

{ TDPLine }

constructor TDPLine.Create(Page: TDPPage);
begin
  inherited Create;
  FFrame := TControlFrame.Create;
  FItemType := TControltype.Line;
  Page.AddItem(TDPControlCustom(Self));
  Height := 0;
  Width := 100;
  FName := 'Line' + inttostr(Page.FCount[FItemType]);
end;

constructor TDPLine.CreateFromNode(Node: IXMLNode; const Page: TDPPage);
var i: integer;
    s, t: string;
begin
  Create(Page);
  for I := 0 to Node.AttributeNodes.Count - 1 do
  begin
    s := uppercase(Node.AttributeNodes[i].NodeName);
    t := Node.AttributeNodes[i].Text;
    if IsControlEntry(s, t) then
    else
    if FFrame.IsFrameEntry(s, t) then
    else
    if s = 'DIAGONAL' then
      FDiagonal := StrToBoolDef(t, True)
    else
      Error.Show('TFrxLineView', Node.AttributeNodes[i].NodeName + ' - ' + t);
  end;
end;

procedure TDPLine.Draw(Canvas: TCanvas; const Zoom: single = 1);
begin
  Canvas.Stroke.Color := FFrame.Color;
  Canvas.Stroke.Kind := FStyle;
  Canvas.Stroke.Thickness := FFrame.PenWidth;

  Canvas.DrawLine(Cliprect.TopLeft, ClipRect.BottomRight , FOpacity);
end;

{ TDPFrame }

constructor TDPShape.Create(Page: TDPPage);
begin
  inherited Create;
  FFrame := TControlFrame.Create;
  FItemType := TControltype.Shape;
  Page.AddItem(TDPControlCustom(Self));
  Height := 18;
  Width := 100;
  FCorners := AllCorners;
  FCornerType := TCornerType.Bevel;
  FName := 'Frame' + inttostr(Page.FCount[FItemType]);
end;

constructor TDPShape.CreateFromNode(Node: IXMLNode; const Page: TDPPage);
var i: integer;
    s, t: string;
begin
  Create(Page);
  for I := 0 to Node.AttributeNodes.Count - 1 do
  begin
    s := uppercase(Node.AttributeNodes[i].NodeName);
    t := Node.AttributeNodes[i].Text;
    if IsControlEntry(s, t) then
    else
    if FFrame.IsFrameEntry(s, t) then //Tool.ShowColor(FFrame.FColor, s+': '+t)
    else
    if s = 'CURVE' then
      SetCornerRadius(StrToFloatDef(t, 0))
    else
    if s = 'SHAPE' then
      SetShape(t)
    else
      Error.Show('TFrxShapeView', s + ' - ' + t);
  end;

end;

procedure TDPShape.Draw(Canvas: TCanvas; const Zoom: single = 1);
var r: TRectF;
begin
  r := clipRect;
  Canvas.Stroke.Color := FFrame.FColor;
  Canvas.Stroke.Kind := FStyle;
  Canvas.Stroke.Thickness := FFrame.PenWidth;

  if Shape = TShapeType.skRoundRectangle then
    Canvas.DrawRect(r, FCornerRadius, FCornerRadius, FCorners, FOpacity, FCornerType);
  if Shape = TShapeType.skRectangle then
    Canvas.DrawRect(r, 0, 0, AllCorners, FOpacity, FCornerType);
  if Shape = TShapeType.skEllipse then
    Canvas.DrawEllipse(r, FOpacity);
end;

function TDPShape.getCornerRadius: single;
begin
  result := FCornerRadius / VFactor;
end;

function TDPShape.getLineWidth: single;
begin
  result := FFrame.Width;
end;

procedure TDPShape.SetCornerRadius(const Value: single);
begin
  FCornerRadius := Value * VFactor;
end;

procedure TDPShape.setLineWidth(const Value: single);
begin
  FFrame.Width := Value;
end;

procedure TDPShape.SetShape(const Value: String);
var s: string;
begin
  s := uppercase(value);
  if s = 'SKRECTANGLE' then
    Shape := TShapeType.skRectangle
  else
  if s = 'SKROUNDRECTANGLE' then
  begin
    Shape := TShapeType.skRoundRectangle;
    FCornerType := TCornerType.Round;
  end
  else
  if s = 'SKELLIPSE' then
    Shape := TShapeType.skEllipse
  else
  if s = 'SKTRIANGLE' then
    Shape := TShapeType.skTriangle
  else
    Error.Show('TFrxShapeView - Shape', value);
end;

procedure TDPShape.setShapeValue(const Value: TShapeType);
begin
  FShape := Value;
end;

{ TDPFont }

constructor TDPFont.Create;
begin
  inherited;
  VFactor := 4.17;
  FColor := TAlphaColors.Black;
  FName := 'System';
  FHeight := 11;
  FStyles := [];
end;

function TDPFont.getHeight: Single;
begin
  result := FHeight;
end;

function TDPFont.IsFontEntry(const Name: string; const Value: string): boolean;
var s: string;
begin
  s:= uppercase(Name);
  result := true;
  if s = 'FONT.CHARSET' then FCharset := StrToIntDef(Value, 0)
  else
  if s = 'FONT.COLOR' then
    FColor := ColorToAlphaColor(Value)
  else
  if s = 'FONT.HEIGHT' then Height := round(abs(StrToIntDef(Value, 0)) * 0.75)
  else
  if s = 'FONT.NAME' then FName := Value
  else
  if s = 'FONT.STYLE' then SetFontStyles(Value)
  else
    result := false;
end;

procedure TDPFont.setFontStyles(value: string);
var i: integer;
begin
  i := strtointdef(value, 0);
  FStyles := [];
  if (i and 1) = 1 then include(FStyles, TFontStyle.fsBold);
  if (i and 2) = 2 then include(FStyles, TFontStyle.fsItalic);
  if (i and 4) = 4 then include(FStyles, TFontStyle.fsUnderline);
  if (i and 8) = 8 then include(FStyles, TFontStyle.fsStrikeOut);
end;

procedure TDPFont.setHeight(const Value: single);
begin
  FHeight := Value;
end;

procedure TDPFont.SetWidth(const Value: single);
begin
  FWidth := Value;
end;

{ TDPPage }

constructor TDPPage.Create(AOwner: TComponent);
begin
  inherited Create;
  FFont := TDPFont.Create;
  PaperWidth := 210.000000000000000000;
  PaperHeight := 297.000000000000000000;
  PaperSize := 9;
  FFont.VFactor := FHeight / PaperHeight;
  FTopMargin := 10;
  FBottomMargin := 10;
  FOrientation := TScreenOrientation.Portrait;
  SetLength(FItems, 0);
  FBackGround := TAlphaColors.White;
end;

constructor TDPPage.CreateFromNode(Node: IXMLNode);
var i: integer;
    s, t: string;
begin
  inherited Create;
  SetLength(FItems, 0);
  FBackGround := TAlphaColors.White;
  FFont := TDPFont.Create;
  for I := 0 to Node.AttributeNodes.Count - 1 do
  begin
    s := uppercase(Node.AttributeNodes[i].NodeName);
    t := Node.AttributeNodes[i].Text;
    if FFont.IsFontEntry(Node.AttributeNodes[i].NodeName,
      Node.AttributeNodes[i].Text) then
    else
    if s = 'NAME' then
      Name := Node.AttributeNodes[i].Text
    else
    if s = 'PAPERWIDTH' then PaperWidth := StrToFloatDef(t, 0)
    else
    if s = 'PAPERHEIGHT' then
      PaperHeight := StrToFloatDef(t, 0)
    else
    if s = 'PAPERSIZE' then
      PaperSize := StrToIntDef(t, 0)
    else
    if s = 'LEFTMARGIN' then
      LeftMargin := StrToFloatDef(t, 0)
    else
    if s = 'RIGHTMARGIN' then
      RightMargin := StrToFloatDef(t, 0)
    else
    if s = 'TOPMARGIN' then
      TopMargin := StrToFloatDef(t, 0)
    else
    if s = 'BOTTOMMARGIN' then
      BottomMargin := StrToFloatDef(t, 0)
    else
    if s = 'COLUMNWIDTH' then
      ColumnWidth := StrToFloatDef(t, 0)
    else
    if s = 'COLUMNPOSITIONS.TEXT' then
      ColumnPositions := t
    else
    if s = 'HGUIDES.TEXT' then
      HGuides := t
    else
    if s = 'VGUIDES.TEXT' then
      VGuides := t
    else
    if s = 'ORIENTATION' then
    begin
      if uppercase(t) = 'POLANDSCAPE' then
        FOrientation := TScreenOrientation.Landscape
      else
        Error.Show('TFrxReportPage - Orientation', t);
    end
    else
      Error.Show('TFrxReportPage', Node.AttributeNodes[i].NodeName + ' - ' +
        Node.AttributeNodes[i].Text);
  end;
  for i := 0 to Node.ChildNodes.Count - 1 do
  begin
    if uppercase(Node.ChildNodes[i].NodeName) = 'TFRXMEMOVIEW' then
      TDPTextBox.CreateFromNode(Node.ChildNodes[i], self)
    else
    if uppercase(Node.ChildNodes[i].NodeName) = 'TFRXLINEVIEW' then
      TDPLine.CreateFromNode(Node.ChildNodes[i], self)
    else
    if uppercase(Node.ChildNodes[i].NodeName) = 'TFRXSHAPEVIEW' then
      TDPShape.CreateFromNode(Node.ChildNodes[i], self)
    else
    if uppercase(Node.ChildNodes[i].NodeName) = 'TFRXPICTUREVIEW' then
      TDPImage.CreateFromNode(Node.ChildNodes[i], self)
    else
      Error.Show('TFrxReportPage - Unknown', Node.ChildNodes[i].NodeName);
  end;
  //showmessage('Items: ' + inttostr(Length(FItems)));
end;

destructor TDPPage.Destroy;
begin
  showmessage('kaputt');
  inherited
end;

procedure TDPPage.Draw(Canvas: TCanvas; const Zoom: single = 1);
var i: integer;
begin
  //showmessage('draw: '+inttostr(length(FItems)));
  for I := Low(FItems) to High(FItems) do
  begin
    if FItems[i] <> nil then
      case FItems[i].FItemType of
        TControlType.TextBox: TDPTextBox(FItems[i]).Draw(Canvas);
        TControlType.Image: TDPImage(FItems[i]).Draw(Canvas);
        TControlType.Line: TDPLine(FItems[i]).Draw(Canvas);
        TControlType.Shape: TDPShape(FItems[i]).Draw(Canvas);
      end;
  end;

end;

function TDPPage.getBottomMargin: single;
begin
  result := FBottomMargin / FHeight * PaperHeight;
end;

function TDPPage.getLeftMargin: single;
begin
  result := FLeftMargin / FWidth * Paperwidth;
end;

function TDPPage.getRightMargin: single;
begin
  result := FRightMargin / FWidth * Paperwidth;
end;

function TDPPage.getTopMargin: single;
begin
  result := FTopMargin / FHeight * PaperHeight;
end;

procedure TDPPage.SetBottomMargin(const Value: single);
begin
  FBottomMargin := Value * FHeight / PaperHeight;
end;

procedure TDPPage.SetLeftMargin(const Value: single);
begin
  FLeftMargin := Value * FWidth / PaperWidth;
end;

procedure TDPPage.SetPaperSize(const Value: integer);
begin
  FPaperSize := Value;
  if FPaperSize = 9 then
  begin
    FHeight := b;
    FWidth := a;
  end;
end;

procedure TDPPage.SetRightMargin(const Value: single);
begin
  FRightMargin := Value * FWidth / PaperWidth;
end;

procedure TDPPage.SetTopMargin(const Value: single);
begin
  FTopMargin := Value * FHeight / PaperHeight;
end;

procedure TDPPage.AddItem(Item: TDPControlCustom);
begin
  SetLength(FItems, Length(FItems) + 1);
  FItems[Length(FItems) - 1] := Item;
  Item.FPage := self;
  inc(FCount[Item.FItemType]);
end;

{ TDPReport }

procedure TDPReport.addPage(const value: TDPPage);
begin
  SetLength(FPages, Length(FPages) + 1);
  FPages[Length(FPages) - 1] := value;
  Pages_.Add(value);
end;

constructor TDPReport.Create;
begin
  inherited Create;
  Pages_ := TObjectList<TDPPage>.Create();
end;

function TDPReport.getPage(Index: integer): TDPPage;
begin
  result := nil;
  if (index >= 0) and (index < Length(FPages)) then
    result := FPages[index];
end;

function TDPReport.LoadFromFR3(const filename: TFileName): boolean;
var f: IXMLDocument;//FR3Document;
    n, n_: IXMLNode;//FR3Node;
    i, j: integer;
    s, t: string;
begin
  result := true;
  f := TXMLDocument.Create(nil);
  try
    f.LoadFromFile(fileName);
    n := f.DocumentElement;

      //raise exception.Create('unexpected file');
  except
    result := false;
  end;
  if (n = nil) or(uppercase(n.NodeName) <> 'TFRXREPORT') then
  begin
    if n = nil then
      s := 'not open'
    else
      s := n.NodeName;
    Error.Show(extractFileName(FileName) , 'Unexpected file. - ' + s);
    exit;
  end;
  for i := 0 to n.AttributeNodes.Count - 1 do
  begin
    s := uppercase(n.AttributeNodes[i].NodeName);
    t := n.AttributeNodes[i].Text;
    if s = 'VERSION' then
      Version := t
    else
    if s = 'DOTMATRIXREPORT' then
      DotMatrixReport := StrToBoolDef(t, false)
    else
    if s = 'INIFILE' then
      IniFile := t
    else
    if s = 'PREVIEWOPTIONS.BUTTONS' then
      PreviewOptions.Buttons := StrToIntDef(t, 0)
    else
    if s = 'PREVIEWOPTIONS.ZOOM' then
      PreviewOptions.Zoom := StrToFloatDef(t, 1)
    else
    if s = 'PRINTOPTIONS.PRINTER' then
      PrintOptions.Printer := t
    else
    if s = 'PRINTOPTIONS.PRINTONSHEET' then
      PrintOptions.PrintOnSheet := StrToIntDef(t, 0)
    else
    if s = 'PRINTOPTIONS.PAGENUMBERS' then
      PrintOptions.PageNumbers := StrToIntDef(t, 0)
    else
    if ReportOptions.IsEntry(s, t) then
    else
      Error.Show('TFrxReport', n.AttributeNodes[i].NodeName + ': ' + t);
  end;
  for i := 0 to n.ChildNodes.Count - 1 do
  begin
    if upperCase(n.ChildNodes[i].NodeName) = 'TFRXDATAPAGE' then
    begin
      if DataPage <> nil then
        Error.Show('Hint', 'Datapage already initialised.')
      else
        DataPage := TDPDataPage.CreateFromNode(n.ChildNodes[i]);
    end
    else
    if upperCase(n.ChildNodes[i].NodeName) = 'TFRXREPORTPAGE' then
    begin
      setLength(FPages, length(FPages) + 1);
      FPages[High(FPages)] := TDPPage.CreateFromNode(n.ChildNodes[i]);
      Pages_.Add(FPages[High(FPages)]);
    end
    else
      Error.Show('TFrxReport - Unexpected item', n.ChildNodes[i].NodeName);

  end;
  //f.Free;
end;

{ TDPDataPage }

constructor TDPDataPage.Create(const Name: string; Top, Left, Height,
  Width: integer);
begin
  inherited Create;
  FName := Name;
  FTop := top;
  FLeft := Left;
  FHeight := Height;
  FWidth := width;
end;

constructor TDPDataPage.CreateFromNode(Node: IXMLNode);
var i: integer;
    s: string;
begin
  for I := 0 to Node.AttributeNodes.Count - 1 do
  begin
    s := uppercase(Node.AttributeNodes[i].NodeName);
    if s = 'NAME' then
      FName := Node.AttributeNodes[i].Text
    else
    if s = 'TOP' then
      FTop := StrToIntDef(Node.AttributeNodes[i].Text, 0)
    else
    if s = 'LEFT' then
      FLeft := StrToIntDef(Node.AttributeNodes[i].Text, 0)
    else
    if s = 'HEIGHT' then
      FHeight := StrToIntDef(Node.AttributeNodes[i].Text, 0)
    else
    if s = 'WIDTH' then
      FWidth := StrToIntDef(Node.AttributeNodes[i].Text, 0)
    else
      Error.Show('TFrxDataPage', Node.AttributeNodes[i].NodeName + ' - ' +
        Node.AttributeNodes[i].Text);
  end;

end;

{ TReportOptions }

function TReportOptions.IsEntry(const Name, Value: string): boolean;
var p: integer;
    s: string;
begin
  p := system.Pos('REPORTOPTIONS.', Name);
  if p <> 0 then
  begin
    result := true;
    s := copy(Name, p + 14);
    if s = 'CREATEDATE' then
      CreateDate := StrToFloatDef(Value, 0)
    else
    if s = 'LASTCHANGE' then
      LastChange := StrToFloatDef(Value, 0)
    else
    if s = 'NAME' then
      roName := Value
    else
    if s = 'DESCRIPTION.TEXT' then
      Description := Value
    else
    if s = 'AUTHOR' then
      Author := Value
    else
    if s = 'VERSIONMAJOR' then
      VersionMajor := strtointdef(Value, 0)
    else
    if s = 'VERSIONMINOR' then
      VersionMinor := strtointdef(Value, 0)
    else
    if s = 'VERSIONRELEASE' then
      VersionRelease := strtointdef(Value, 0)
    else
    begin
      result := false;
    end;
  end;
end;

{ TControlFrame }

constructor TControlFrame.Create;
var i : TSide;
begin
  for i  in AllSides do
  begin
    Line[i].Color := TAlphaColors.Black;
    Line[i].Style := TBrushKind.Solid;
    Line[i].Width := 1;
  end;
  FWidth := 1;
  FColor := TAlphaColors.Black;
end;

procedure TControlFrame.Draw(Canvas: TCanvas; const ClipRect: TRectF; const Opacity: single = 1);
var p: TPointF;

  procedure setPen(const value: TSide);
  begin
    Canvas.Stroke.Color := Line[value].Color;
    Canvas.Stroke.Kind := Line[value].Style;
    Canvas.Stroke.Thickness := Line[value].Width * LineCorrection;
    if Line[value].Width > 0 then
      Canvas.Stroke.Thickness := Canvas.Stroke.Thickness + 2;

  end;

begin
  if TSide.Top in Typ then
  begin
    p.X := ClipRect.Right;
    p.Y := ClipRect.Top;
    setPen(TSide.Top);
    Canvas.DrawLine(ClipRect.TopLeft, p, Opacity);
  end;
  if TSide.Right in Typ then
  begin
    p.X := ClipRect.Right;
    p.Y := ClipRect.Top;
    SetPen(TSide.Right);
    Canvas.DrawLine(p, ClipRect.BottomRight, Opacity);
  end;
  if TSide.Bottom in Typ then
  begin
    p.X := ClipRect.Left;
    p.Y := ClipRect.Bottom;
    SetPen(TSide.Bottom);
    Canvas.DrawLine(ClipRect.BottomRight ,p , Opacity);
  end;
  if TSide.Left in Typ then
  begin
    p.X := ClipRect.Left;
    p.Y := ClipRect.Bottom;
    SetPen(TSide.Left);
    Canvas.DrawLine(ClipRect.TopLeft, p, Opacity);
  end;
end;

procedure TControlFrame.DrawGuide(Canvas: TCanvas; const ClipRect: TRectF;
  const Opacity: single = 1);

const long: single = 20;
var p, d: TPointF;

begin
  Canvas.Stroke.Color := TAlphaColors.Gray;
  Canvas.Stroke.Kind := TBrushKind.Solid;;
  Canvas.Stroke.Thickness := 2;
    begin
      p.X := ClipRect.Left + long;
      p.Y := ClipRect.Top;
      Canvas.DrawLine(ClipRect.TopLeft, p, Opacity);
      p.X := ClipRect.Left;
      p.Y := ClipRect.Top + long;
      Canvas.DrawLine(ClipRect.TopLeft, p, Opacity);
    end;
    begin
      p.X := ClipRect.Right - long;
      p.Y := ClipRect.Bottom;
      Canvas.DrawLine(p, ClipRect.BottomRight, Opacity);
      p.X := ClipRect.Right;
      p.Y := ClipRect.Bottom - long;
      Canvas.DrawLine(ClipRect.BottomRight, p, Opacity);
    end;
    begin
      d.X := ClipRect.Left;
      d.Y := ClipRect.Bottom;
      p.X := ClipRect.Left + long;
      p.Y := ClipRect.Bottom;
      Canvas.DrawLine(d ,p , Opacity);
      p.X := ClipRect.Left;
      p.Y := ClipRect.Bottom - long;
      Canvas.DrawLine(d ,p , Opacity);
    end;
    begin
      d.X := ClipRect.Right;
      d.Y := ClipRect.Top;
      p.X := ClipRect.Right - long;
      p.Y := ClipRect.Top;
      Canvas.DrawLine(d, p, Opacity);
      p.X := ClipRect.Right;
      p.Y := ClipRect.Top + long;
      Canvas.DrawLine(d, p, Opacity);
    end;
end;

function TControlFrame.getTyp: integer;
begin
result := 0;
  if TSide.Left in Typ then
    result := 1;
  if TSide.Right in Typ then
    result := result or 2;
  if TSide.Top in Typ then
    result := 4;
  if TSide.Bottom in Typ then
    result := result or 8;
end;

function TControlFrame.IsFrameEntry(const Name, Value: string): boolean;
var p: integer;
    s: string;
begin
  p := system.Pos('FRAME.', Name);
  if p <> 0 then
  begin
    result := true;
    s := copy(Name, p + 6);
    if s = 'TYP' then
      self.Value := StrToIntDef(Value, 0)
    else
    if s = 'WIDTH' then
      Width := StrToFloatDef(Value, 1)
    else
    if s = 'COLOR' then
    begin
      //tool.ShowColor(TColor(strtoint(value)));
      FColor := ColorToAlphaColor(Value)
    end
    else
    if s = 'RIGHTLINE.WIDTH' then
      Line[Tside.Right].Width := StrToFloatDef(Value, 1)
    else
    if s = 'LEFTLINE.WIDTH' then
      Line[TSide.Left].Width := StrToFloatDef(Value, 1)
    else
    if s = 'TOPLINE.WIDTH' then
      Line[TSide.Top].Width := StrToFloatDef(Value, 1)
    else
    if s = 'BOTTOMLINE.WIDTH' then
      Line[TSide.Bottom].Width := StrToFloatDef(Value, 1)
    else
      Error.Show('Unknown Frame Item', s);
  end
  else
    result := false;
end;

function TControlFrame.PenWidth: single;
begin
  result := FWidth * LineCorrection;
  if result > 0 then result := result + 2;
end;

procedure TControlFrame.setTyp(const Value: integer);
begin
  if Value and 1 = 1 then
    include(typ, TSide.Left);
  if Value and 2 = 2 then
    include(typ, TSide.Right);
  if Value and 4 = 4 then
    include(typ, TSide.Top);
  if Value and 8 = 8 then
    include(typ, TSide.Bottom);
end;

procedure TControlFrame.setWidth(const Value: single);
var i : TSide;
begin
  for i  in AllSides do
  begin
    Line[i].Width := Value;
  end;
  FWidth := Value;
end;

end.
