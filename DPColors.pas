unit DPColors;

///////////////////////////////////////////////////////////////////////////////
//
//  Unit for dealing with colour conversion
//
//  2018 Bombinho
//
///////////////////////////////////////////////////////////////////////////////

interface

uses System.UITypes, System.SysUtils;

type

  ColorScheme = Array[0..29] of TColor;

Const
  Win8_1_Windows: ColorScheme = ($B4B4B4, $99B4D1, $000000, $ABABAB,
                $F0F0F0, $A0A0A0, $696969, $E3E3E3, $FFFFFF, $000000,
                $000000, $B9D1EA, $D7E4F2, $6D6D6D, $3399FF, $FFFFFF,
                $0066CC, $F4F7FC, $BFCDDB, $000000, $FFFFE1, $000000,
                $F0F0F0, $F0F0F0, $3399FF, $000000, $C8C8C8, $FFFFFF,
                $646464, $000000);
  Win7_Aero: ColorScheme = ($B4B4B4, $99B4D1, $000000, $ABABAB,
                $F0F0F0, $A0A0A0, $696969, $E3E3E3, $FFFFFF, $000000,
                $000000, $B9D1EA, $D7E4F2, $6D6D6D, $3399FF, $FFFFFF,
                $0066CC, $F4F7FC, $BFCDDB, $434E54, $FFFFE1, $000000,
                $F0F0F0, $F0F0F0, $3399FF, $000000, $C8C8C8, $FFFFFF,
                $646464, $000000);
  WinVis_ClassicStd: ColorScheme = ($D4D0C8, $0A246A, $FFFFFF, $808080,
                $D4D0C8, $808080, $404040, $D4D0C8, $FFFFFF, $000000,
                $3A6EA5, $A6CAF0, $C0C0C0, $808080, $0A246A, $FFFFFF,
                $0066CC, $D4D0C8, $808080, $D4D0C8, $FFFFE1, $000000,
                $D4D0C8, $F0F0F0, $3399FF, $000000, $D4D0C8, $FFFFFF,
                $000000, $000000);
  WinXP_Luna: ColorScheme = ($D4D0C8, $0054E3, $FFFFFF, $808080,
                $ECE9D8, $ACA899, $716F64, $F1EFE2, $FFFFFF, $000000,
                $004E98, $3D95FF, $9DB9EB, $ACA899, $316AC5, $FFFFFF,
                $000080, $D4D0C8, $7A96DF, $D8E4F8, $FFFFE1, $000000,
                $FFFFFF, $ECE9D8, $316AC5, $000000, $D4D0C8, $FFFFFF,
                $000000, $000000);
  WinXP_Royal: ColorScheme = ($D4D0C8, $335EA8, $FFFFFF, $808080,
                $EBE9ED, $A7A6AA, $85878C, $DCDFE4, $FFFFFF, $000000,
                $000040, $70B1EB, $83B7E3, $A7A6AA, $335EA8, $FFFFFF,
                $000080, $D4D0C8, $6FA1D9, $FFFFFF, $FFFFE1, $000000,
                $FFFFFF, $EBE9ED, $335EA8, $000000, $D4D0C8, $FFFFFF,
                $000000, $000000);
  WinXP_Embedded: ColorScheme = ($D4D0C8, $3E9DE8, $FFFFFF, $808080,
                $EBE9ED, $B5B4B8, $85878C, $DCDFE4, $FFFFFF, $000000,
                $000040, $70B1EB, $83B7E3, $B5B4B8, $3E9DE8, $FFFFFF,
                $000080, $D4D0C8, $6FA1D9, $FFFFFF, $FFFFE1, $000000,
                $FFFFFF, $EBE9ED, $3E9DE8, $000000, $D4D0C8, $FFFFFF,
                $000000, $000000);

  function GetSysColor(const Value: integer; const Scheme: ColorScheme): TColorRec;
  function ColorToAlphaColor(const value: string): TAlphaColor;

implementation


function GetSysColor(const Value: integer; const Scheme: ColorScheme): TColorRec;
var i: integer;
begin
  case value of
    0:  result.Color := Scheme[26];     // 'clScrollBar';
    1:  result.Color := Scheme[10];     // 'clBackground';
    2:  result.Color := Scheme[1 ];     // 'clActiveCaption';
    3:  result.Color := Scheme[18];     // 'clInactiveCaption';
    4:  result.Color := Scheme[22];     // 'clMenu';
    5:  result.Color := Scheme[27];     // 'clWindow';
    6:  result.Color := Scheme[28];     // 'clWindowFrame';
    7:  result.Color := Scheme[25];     // 'clMenuText';
    8:  result.Color := Scheme[29];     // 'clWindowText';
    9:  result.Color := Scheme[2 ];     // 'clCaptionText';
    10: result.Color := Scheme[0 ];     // 'clActiveBorder';
    11: result.Color := Scheme[17];     // 'clInactiveBorder';
    12: result.Color := Scheme[3 ];     // 'clAppWorkSpace';
    13: result.Color := Scheme[14];     // 'clHighlight';
    14: result.Color := Scheme[15];     // 'clHighlightText';
    15: result.Color := Scheme[4 ];     // 'clBtnFace';
    16: result.Color := Scheme[5 ];     // 'clBtnShadow';
    17: result.Color := Scheme[13];     // 'clGrayText';
    18: result.Color := Scheme[9 ];     // 'clBtnText';
    19: result.Color := Scheme[19];     // 'clInactiveCaptionText';
    20: result.Color := Scheme[8 ];     // 'clBtnHighlight';
    21: result.Color := Scheme[6 ];     // 'cl3DDkShadow';
    22: result.Color := Scheme[7 ];     // 'cl3DLight';
    23: result.Color := Scheme[21];     // 'clInfoText';
    24: result.Color := Scheme[20];     // 'clInfoBk';
    26: result.Color := Scheme[16];     // 'clHotLight';
    27: result.Color := Scheme[11];     // 'clGradientActiveCaption';
    28: result.Color := Scheme[12];     // 'clGradientInactiveCaption';
    29: result.Color := Scheme[24];     // 'clMenuHighlight';
    30: result.Color := Scheme[23];     // 'clMenuBar';
  end;
  i := result.Color;
  result.A := i shr 24;
  result.R := (i shr 16) and $FF;
  result.G := (i shr 8) and $FF;
  result.B := i and $FF;
  //result.A := 255;
end;

function ColorToAlphaColor(const value: string): TAlphaColor;
var
   CRec: TColorRec;
   ARec: TAlphaColorRec;
begin
  if value = '0' then
    result := TAlphaColors.Black
  else
  begin
    CRec.Color := strToIntDef(Value, 0);
    if CRec.Color < 0 then
      CRec := GetSysColor(CRec.Color and $FF, Win7_Aero);
    ARec.A := 255;
    ARec.B := CRec.B;
    ARec.G := CRec.G;
    ARec.R := CRec.R;
    Result := ARec.Color;
  end;
end;

end.
