unit DPPreview;

//////////////////////////////////////////////////////////////////////////////
//
// Colour Tool,  2018 bombinho
//
// Version 0.1
//
//////////////////////////////////////////////////////////////////////////////

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants, 
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  FMX.Layouts, FMX.Controls.Presentation, FMX.Objects, FMX.Edit,
  DPControls;

type

  TPaperSize = class
    FOrientation: TFormOrientation;
    function getSizeA4: TPointF;
  public
    property Orientation: TFormOrientation read FOrientation write FOrientation;
    property A4: TPointF read getSizeA4;
//    property WithInCm: single
//    property HeightInCm: single;
    function Resolution(const SizeInMm: Single; const DPI: Integer): single;
    function ScaleToMm(const SizeInMm: Single; const WindowSize: Integer): single;
  end;

  TPreview = class(TFrame)
    Page: TLayout;
    PaintBox1: TPaintBox;
    procedure FrameResize(Sender: TObject);
    procedure PaintBox1Paint(Sender: TObject; Canvas: TCanvas);
  private
    FCurrentPage: TDPPage;
  public
    Report1: TDPReport;
    property CurrentPage: TDPPage read FCurrentPage write FCurrentPage;
    procedure swap;
    function PrintRect(const PageWidth: integer): TRectF;
    constructor Create(AOwner: TComponent);
  end;

implementation

{$R *.fmx}

{ TFrame2 }

constructor TPreview.Create(AOwner: TComponent);
var p: TDPPage;
begin
  inherited Create(AOwner);
  Report1 := TDPReport.Create;
  P := TDPPage.Create(AOwner);
  Report1.addPage(P);
  P.Name := 'Seite1';
  CurrentPage := p;
  Page.Height := P.Height;
  Page.Width := P.Width;
end;

procedure TPreview.FrameResize(Sender: TObject);
begin
 Page.Height := Height;
// Page.Width := Height / FPapersize.X * FPaperSize.Y;
end;

procedure TPreview.PaintBox1Paint(Sender: TObject; Canvas: TCanvas);
begin
  if Report1.Pages_.Count > 0 then
  begin
    Canvas.Stroke.Kind:= tBrushKind.Solid;
    Canvas.Fill.Color := CurrentPage.BackGroundColor;
    Canvas.FillRect(PaintBox1.BoundsRect, 1,1, AllCorners, 1);
    CurrentPage.Draw(Canvas);
  end;
end;

procedure TPreview.swap;
var i : integer;
begin
  if Report1.Pages_.Count > 1 then
    if CurrentPage = Report1.Pages_[0] then
      CurrentPage := Report1.Pages_[Report1.Pages_.Count - 1]
    else
      CurrentPage := Report1.Pages_[0];
  PaintBox1.Repaint;
end;

{ TPaperSize }

function TPaperSize.getSizeA4;
begin
  if FOrientation in [TScreenOrientation.soPortrait,
    TScreenOrientation.soInvertedPortrait] then
  begin
//    result.Create(210, 297);
  end
  else
  begin
//    result.Create(297, 210);
  end;
end;

function TPaperSize.Resolution(const SizeInMm: Single;const DPI: Integer): single;
begin
  result := SizeInMm * 0.254 * DPI;
end;

function TPaperSize.ScaleToMm(const SizeInMm: Single; const WindowSize: Integer): single;
begin
  result := WindowSize / SizeInMm;
end;


function TPreview.PrintRect(const PageWidth: integer): TRectF;
var corr: single;
begin
  if (PageWidth >4500) and (PageWidth < 5500) then
    corr := round((4961 - PageWidth) / 4)
  else
    corr := 0;
  result := PaintBox1.LocalRect;
  result.TopLeft.X := result.TopLeft.X + corr;
  result.TopLeft.Y := result.TopLeft.Y + corr;
  result.BottomRight.X := result.BottomRight.X - corr;
  result.BottomRight.Y := result.BottomRight.Y - corr;
end;

end.

