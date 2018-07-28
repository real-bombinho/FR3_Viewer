unit Main;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  FMX.Controls.Presentation, FMX.StdCtrls, FMX.ListBox, FMX.Printer,
  DPPreview, DPControls, FMX.Objects, Xml.xmldom, Xml.XmlTransform,
  System.ImageList, FMX.ImgList, FMX.Menus;

type
  TForm1 = class(TForm)
    ToolBar1: TToolBar;

    SpeedButton1: TSpeedButton;
    SpeedButton2: TSpeedButton;
    ZoomBox: TComboBox;
    PrintDialog1: TPrintDialog;
    ScrollVert: TScrollBar;
    Image1: TImage;
    XMLTransform1: TXMLTransform;
    OpenDialog1: TOpenDialog;
    SpeedButton3: TSpeedButton;
    SpeedButton4: TSpeedButton;
    ImageList1: TImageList;

    procedure FormCreate(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure Preview1Resize(Sender: TObject);
    procedure SpeedButton2Click(Sender: TObject);
    procedure SpeedButton1Click(Sender: TObject);
    procedure ZoomBoxChange(Sender: TObject);
    procedure ToolBar1Resize(Sender: TObject);
    procedure SpeedButton3Click(Sender: TObject);
    procedure SpeedButton4Click(Sender: TObject);
  private
    PreView1: TPreview;
    tb1: TDPTextBox;
    rp: TDPReport;
    procedure setPageZoom(const zoom: single);

  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.fmx}

procedure TForm1.FormCreate(Sender: TObject);
var tb: TDPTextBox;
    im: TDPImage;
    ln: TDPLine;
    fr: TDPShape;
    var i: integer;
begin
  rp:= TDPReport.Create;

  Preview1 := TPreview.Create(Application);
  Preview1.Width := ClientWidth - 60;
  Preview1.Position.X := 20;
  Preview1.Position.Y := Toolbar1.Height + 10;
  Preview1.Height := ClientHeight - 40;
  Preview1.Parent := Form1;

  tb1 := TDPTextBox.Create(Preview1.CurrentPage);
  tb1.Left := 30;
  tb1.Top := 30;
  tb1.BoxColor := 0;//TAlphaColors.White;
  tb1.frame.Typ := AllSides;
  tb1.Text := 'TextBox';
  tb1.Opacity := 0.5;
  tb1.Frame.Color := TAlphaColors.Black;

  tb := TDPTextBox.Create(Preview1.CurrentPage);
  tb.Top := 10;
  tb.Left := 10;
  tb.Color := TAlphaColors.Black;//.Red;
  tb.Font.Height := round(tb.Height);
  tb.BoxColor := 0;//TAlphaColors.Yellow;
  tb.frame.Typ := AllSides;
  tb.Text := 'TextBox2';

  im := TDPImage.Create(Preview1.CurrentPage);
  im.Left := 40;
  im.Top := 45;
  im.Transparent := true;
  im.BitMap := Image1.Bitmap;

  ln := TDPLine.Create(Preview1.CurrentPage);
  ln.Left := 35;
  ln.Top := 35;
  ln.Height := ln.Width;

  for i := 0 to 42 do
  begin
    ln := TDPLine.Create(Preview1.CurrentPage);
    ln.Left := i*5;
    ln.Top := 0;
    ln.Width := 0;
    ln.Frame.Width := 1 + (i mod 6);
    ln.Height := 10 -(5 * (i mod 2));
  end;

  preview1.CurrentPage.LeftMargin := 0;
  fr := TDPShape.Create(Preview1.CurrentPage);
  fr.left := 10;
  fr.Top := 10;
  fr.Height := 100;
  fr.Width := 100;
  fr.Shape := TShapeType.skRoundRectangle;
  fr.CornerType := TCornerType.Round;
  fr.Frame.Color := TAlphaColors.Green;
  fr.CornerRadius := 30;

  ZoomBox.Items.Clear;
  ZoomBox.Items.Add('25%');
  ZoomBox.Items.Add('50%');


  ZoomBox.Items.Add('75%');
  ZoomBox.Items.Add('100%');
  ZoomBox.Items.Add('150%');
  ZoomBox.Items.Add('200%');
  ZoomBox.Items.Add('PageWidth');
  ZoomBox.Items.Add('WholePage');
  ZoomBox.ItemIndex := 3;

//  showmessage(fr.Name);
end;

procedure TForm1.FormResize(Sender: TObject);
begin
  Preview1.Height := ClientHeight - 30 - ToolBar1.Height;
  Preview1.Position.X := 15 + ToolBar1.Height;
end;

{procedure TForm1.Preview1Panel1Paint(Sender: TObject; Canvas: TCanvas);
var i: integer;
begin
//  with preview1.FPanel1.Canvas do
  begin
    showmessage('canvas ' + inttostr(integer(preview1.PaintBox1.Canvas)));
//    stroke.color := TAlphaColors.Grey;
//    Stroke.Thickness := 2;
 //   Stroke.Kind := TBrushKind.Solid;
 //   Fill.Kind := TBrushKind.Solid;
 //   Fill.Color := TAlphaColors.White;
 //   FillRect(preview1.Panel1.ClipRect, 0, 0, AllCorners, 100);
  end;
//  For i := Low(Preview1.TextBoxes) to high(Preview1.TextBoxes) do
//    Preview1.TextBoxes[i].Draw(preview1.Panel1.canvas);
//  For i := Low(Preview1.Images) to high(Preview1.Images) do
//    Preview1.Images[i].Draw(preview1.Panel1.canvas);
//  Preview1.show;
end; }


procedure TForm1.Preview1Resize(Sender: TObject);
begin
  Preview1.FrameResize(Sender);
  Preview1.Page.Height := Preview1.Height;
  Preview1.Page.Width := Height / 1.414;
  Preview1.Page.Scale.X := 1;
  Preview1.Page.Scale.Y := 1;
end;

procedure TForm1.SpeedButton1Click(Sender: TObject);
begin
  tb1.Left := tb1.Left + 2;
  invalidate;
end;

procedure TForm1.SpeedButton2Click(Sender: TObject);
var
  SrcRect, DestRect: TRectF;
  f: single;
  i: integer;
  pntMargins : TPoint;
begin

  if printdialog1.Execute then
  begin

//  for i := 0 to Printer.Count - 1 do
//    showmessage(Printer.printers[i].Device);
    Printer.ActivePrinter.SelectDPI(1200, 1200);
    Printer.Canvas.Fill.Color := TAlphaColors.Black;
    Printer.Canvas.Fill.Kind := TBrushKind.Solid;
    Printer.Canvas.Stroke.Color := TAlphaColors.Black;
    Printer.Canvas.Stroke.Kind := TBrushKind.Solid;
    Printer.BeginDoc;
    SrcRect := Preview1.PrintRect(Printer.PageWidth);
    DestRect := TRectF.Create(0, 0, Printer.PageWidth, Printer.PageHeight);
    Printer.Canvas.DrawBitmap(Preview1.PaintBox1.MakeScreenshot, SrcRect, DestRect, 1);
    Printer.EndDoc;

  end;
end;

procedure TForm1.SpeedButton3Click(Sender: TObject);
begin
  OpenDialog1.InitialDir := ExtractFilePath(ParamStr(0)+'\..\..');
  if OpenDialog1.Execute then
  begin
    Preview1.Report1.LoadFromFR3(OpenDialog1.FileName);
    Preview1.CurrentPage := Preview1.Report1.Pages_[Preview1.Report1.Pages_.Count-1];
    Preview1.PaintBox1.Repaint;
  end;
end;

procedure TForm1.SpeedButton4Click(Sender: TObject);
begin
  Preview1.swap;
  Preview1.Page.Repaint;
end;

procedure TForm1.ToolBar1Resize(Sender: TObject);
begin
  if Preview1 <> nil then
    Preview1.Height := Clientheight - Preview1.Position.X - 20;
end;

procedure TForm1.ZoomBoxChange(Sender: TObject);
var r: double;
begin
  case zoombox.ItemIndex of
    0: r := 0.25;
    1: r := 0.5;
    2: r := 0.75;
    3: r := 1;
    4: r := 1.25;
    5: r := 1.5;
    6: r := 1;
    7: r := 0.6;
  end;
  Preview1.page.scale.x := r;
  Preview1.page.scale.y := r;
end;

procedure TForm1.setPageZoom(const zoom: Single);
begin
  Preview1.Width := ClientWidth - ScrollVert.Width;
end;

end.
