unit BGRAJSONStyles;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, BGRABitmap, BGRABitmapTypes, fpjson, Math, LCLType;

type

  { TBGRAJSONRoundRect }

  TRoundRectangleStyle = (rrsRounded, rrsSquare, rrsBevel);
  TMeasurementUnit = (muPixels, muPercent);

  TBGRAJSONRoundRect = class
  private
    FBackgroundColor: TBGRAPixel;
    FBorderBottomLeftStyle: TRoundRectangleStyle;
    FBorderBottomRightStyle: TRoundRectangleStyle;
    FBorderColor: TBGRAPixel;
    FBorderTopLeftStyle: TRoundRectangleStyle;
    FBorderTopRightStyle: TRoundRectangleStyle;
    FBorderWidth: single;
    FBorderXRadius: single;
    FBorderXRadiusUnit: TMeasurementUnit;
    FBorderYRadius: single;
    FBorderYRadiusUnit: TMeasurementUnit;
    FHeight: single;
    FHeightUnit: TMeasurementUnit;
    FStyle: string;
    FWidth: single;
    FBorderOptions: TRoundRectangleOptions;
    FWidthUnit: TMeasurementUnit;
    procedure SetBackgroundColor(AValue: TBGRAPixel);
    procedure SetBorderBottomLeftStyle(AValue: TRoundRectangleStyle);
    procedure SetBorderBottomRightStyle(AValue: TRoundRectangleStyle);
    procedure SetBorderColor(AValue: TBGRAPixel);
    procedure SetBorderTopLeftStyle(AValue: TRoundRectangleStyle);
    procedure SetBorderTopRightStyle(AValue: TRoundRectangleStyle);
    procedure SetBorderWidth(AValue: single);
    procedure SetBorderXRadius(AValue: single);
    procedure SetBorderXRadiusUnit(AValue: TMeasurementUnit);
    procedure SetBorderYRadius(AValue: single);
    procedure SetBorderYRadiusUnit(AValue: TMeasurementUnit);
    procedure SetHeight(AValue: single);
    procedure SetHeightUnit(AValue: TMeasurementUnit);
    procedure SetStyle(AValue: string);
    procedure SetWidth(AValue: single);
    procedure SetWidthUnit(AValue: TMeasurementUnit);
  protected
    procedure ProcessJSON;
    procedure ProcessBorderTopLeftStyle;
    procedure ProcessBorderBottomLeftStyle;
    procedure ProcessBorderTopRightStyle;
    procedure ProcessBorderBottomRightStyle;
  public
    constructor Create;
  public
    procedure LoadFromFile(FileName: string);
  public
    procedure Draw(DestBitmap: TBGRABitmap; DestRect: TRect);
  public
    property Width: single read FWidth write SetWidth;
    property Height: single read FHeight write SetHeight;
    property WidthUnit: TMeasurementUnit read FWidthUnit write SetWidthUnit;
    property HeightUnit: TMeasurementUnit read FHeightUnit write SetHeightUnit;
    property BackgroundColor: TBGRAPixel read FBackgroundColor write SetBackgroundColor;
    property BorderColor: TBGRAPixel read FBorderColor write SetBorderColor;
    property BorderWidth: single read FBorderWidth write SetBorderWidth;
    property BorderXRadius: single read FBorderXRadius write SetBorderXRadius;
    property BorderYRadius: single read FBorderYRadius write SetBorderYRadius;
    property BorderXRadiusUnit: TMeasurementUnit
      read FBorderXRadiusUnit write SetBorderXRadiusUnit;
    property BorderYRadiusUnit: TMeasurementUnit
      read FBorderYRadiusUnit write SetBorderYRadiusUnit;
    property BorderTopLeftStyle: TRoundRectangleStyle
      read FBorderTopLeftStyle write SetBorderTopLeftStyle;
    property BorderTopRightStyle: TRoundRectangleStyle
      read FBorderTopRightStyle write SetBorderTopRightStyle;
    property BorderBottomLeftStyle: TRoundRectangleStyle
      read FBorderBottomLeftStyle write SetBorderBottomLeftStyle;
    property BorderBottomRightStyle: TRoundRectangleStyle
      read FBorderBottomRightStyle write SetBorderBottomRightStyle;
  public
    property Style: string read FStyle write SetStyle;
  end;

implementation

procedure JSONReadSingle(propName: string; var jObj: TJSONObject; out floatVar: single);
var
  jTmp: TJSONData;
begin
  jTmp := jObj.FindPath(propName);
  if Assigned(jTmp) then
    floatVar := jTmp.AsFloat;
end;

procedure JSONReadColor(propName: string; var jObj: TJSONObject;
  out colorVar: TBGRAPixel);
var
  jTmp: TJSONData;
begin
  jTmp := jObj.FindPath(propName);
  if Assigned(jTmp) then
    colorVar := StrToBGRA(jTmp.AsString);
end;

function JSONReadBorderStyle(propName: string; var jObj: TJSONObject;
  out styleVar: TRoundRectangleStyle): boolean;
var
  jTmp: TJSONData;
begin
  jTmp := jObj.FindPath(propName);
  Result := Assigned(jTmp);
  if Result then
    case jTmp.AsString of
      'rounded': styleVar := rrsRounded;
      'bevel': styleVar := rrsBevel;
      'square': styleVar := rrsSquare;
      else
        styleVar := rrsSquare;
    end;
end;

function JSONReadMeasurementUnit(propName: string; var jObj: TJSONObject;
  out unitVar: TMeasurementUnit): boolean;
var
  jTmp: TJSONData;
begin
  jTmp := jObj.FindPath(propName);
  Result := Assigned(jTmp);
  if Result then
    case jTmp.AsString of
      '%': unitVar := muPercent;
      'px': unitVar := muPixels;
      else
        unitVar := muPixels;
    end;
end;

procedure JSONReadSingleMeasurement(propName: string; var jObj: TJSONObject;
  out floatVar: single; out unitVar: TMeasurementUnit);
var
  jTmp: TJSONData;
  sTmp: string;
begin
  jTmp := jObj.FindPath(propName);
  if Assigned(jTmp) then
  begin
    sTmp := jTmp.AsString;
    if Pos('px', sTmp) <> 0 then
    begin
      unitVar := muPixels;
      sTmp := StringReplace(sTmp, 'px', '', [rfReplaceAll]);
    end
    else if Pos('%', sTmp) <> 0 then
    begin
      unitVar := muPercent;
      sTmp := StringReplace(sTmp, '%', '', [rfReplaceAll]);
    end
    else
      unitVar := muPixels;
    floatVar := StrToFloat(sTmp);
  end;
end;

{ TBGRAJSONRoundRect }

procedure TBGRAJSONRoundRect.SetBackgroundColor(AValue: TBGRAPixel);
begin
  if FBackgroundColor = AValue then
    Exit;
  FBackgroundColor := AValue;
end;

procedure TBGRAJSONRoundRect.SetBorderBottomLeftStyle(AValue: TRoundRectangleStyle);
begin
  if FBorderBottomLeftStyle = AValue then
    Exit;
  FBorderBottomLeftStyle := AValue;
  ProcessBorderBottomLeftStyle;
end;

procedure TBGRAJSONRoundRect.SetBorderBottomRightStyle(AValue: TRoundRectangleStyle);
begin
  if FBorderBottomRightStyle = AValue then
    Exit;
  FBorderBottomRightStyle := AValue;
  ProcessBorderBottomRightStyle;
end;

procedure TBGRAJSONRoundRect.SetBorderColor(AValue: TBGRAPixel);
begin
  if FBorderColor = AValue then
    Exit;
  FBorderColor := AValue;
end;

procedure TBGRAJSONRoundRect.SetBorderTopLeftStyle(AValue: TRoundRectangleStyle);
begin
  if FBorderTopLeftStyle = AValue then
    Exit;
  FBorderTopLeftStyle := AValue;
  ProcessBorderTopLeftStyle;
end;

procedure TBGRAJSONRoundRect.SetBorderTopRightStyle(AValue: TRoundRectangleStyle);
begin
  if FBorderTopRightStyle = AValue then
    Exit;
  FBorderTopRightStyle := AValue;
  ProcessBorderTopRightStyle;
end;

procedure TBGRAJSONRoundRect.SetBorderWidth(AValue: single);
begin
  if FBorderWidth = AValue then
    Exit;
  FBorderWidth := AValue;
end;

procedure TBGRAJSONRoundRect.SetBorderXRadius(AValue: single);
begin
  if FBorderXRadius = AValue then
    Exit;
  FBorderXRadius := AValue;
end;

procedure TBGRAJSONRoundRect.SetBorderXRadiusUnit(AValue: TMeasurementUnit);
begin
  if FBorderXRadiusUnit = AValue then
    Exit;
  FBorderXRadiusUnit := AValue;
end;

procedure TBGRAJSONRoundRect.SetBorderYRadius(AValue: single);
begin
  if FBorderYRadius = AValue then
    Exit;
  FBorderYRadius := AValue;
end;

procedure TBGRAJSONRoundRect.SetBorderYRadiusUnit(AValue: TMeasurementUnit);
begin
  if FBorderYRadiusUnit = AValue then
    Exit;
  FBorderYRadiusUnit := AValue;
end;

procedure TBGRAJSONRoundRect.SetHeight(AValue: single);
begin
  if FHeight = AValue then
    Exit;
  FHeight := AValue;
end;

procedure TBGRAJSONRoundRect.SetHeightUnit(AValue: TMeasurementUnit);
begin
  if FHeightUnit = AValue then
    Exit;
  FHeightUnit := AValue;
end;

procedure TBGRAJSONRoundRect.SetStyle(AValue: string);
begin
  if FStyle = AValue then
    Exit;
  FStyle := AValue;
  ProcessJSON;
end;

procedure TBGRAJSONRoundRect.SetWidth(AValue: single);
begin
  if FWidth = AValue then
    Exit;
  FWidth := AValue;
end;

procedure TBGRAJSONRoundRect.SetWidthUnit(AValue: TMeasurementUnit);
begin
  if FWidthUnit = AValue then
    Exit;
  FWidthUnit := AValue;
end;

procedure TBGRAJSONRoundRect.ProcessJSON;
var
  jObj: TJSONObject = nil;
begin
  try
    jObj := TJSONObject(GetJSON(FStyle));
    { Size }
    JSONReadSingleMeasurement('width', jObj, FWidth, FWidthUnit);
    JSONReadSingleMeasurement('height', jObj, FHeight, FHeightUnit);
    { Border }
    JSONReadSingle('border-width', jObj, FBorderWidth);
    JSONReadSingleMeasurement('border-x-radius', jObj, FBorderXRadius,
      FBorderXRadiusUnit);
    JSONReadSingleMeasurement('border-y-radius', jObj, FBorderYRadius,
      FBorderYRadiusUnit);
    JSONReadColor('border-color', jObj, FBorderColor);
    if JSONReadBorderStyle('border-top-left-style', jObj, FBorderTopLeftStyle) then
      ProcessBorderTopLeftStyle;
    if JSONReadBorderStyle('border-top-right-style', jObj, FBorderTopRightStyle) then
      ProcessBorderTopRightStyle;
    if JSONReadBorderStyle('border-bottom-left-style', jObj, FBorderBottomLeftStyle) then
      ProcessBorderBottomLeftStyle;
    if JSONReadBorderStyle('border-bottom-right-style', jObj,
      FBorderBottomRightStyle) then
      ProcessBorderBottomRightStyle;
    { Background }
    JSONReadColor('background-color', jObj, FBackgroundColor);
  finally
    if Assigned(jObj) then
      jObj.Free;
  end;
end;

procedure TBGRAJSONRoundRect.ProcessBorderTopLeftStyle;
begin
  case FBorderTopLeftStyle of
    TRoundRectangleStyle.rrsRounded:
    begin
      Exclude(FBorderOptions, rrTopLeftSquare);
      Exclude(FBorderOptions, rrTopLeftBevel);
    end;
    TRoundRectangleStyle.rrsBevel:
    begin
      Exclude(FBorderOptions, rrTopLeftSquare);
      Include(FBorderOptions, rrTopLeftBevel);
    end;
    TRoundRectangleStyle.rrsSquare:
    begin
      Include(FBorderOptions, rrTopLeftSquare);
      Exclude(FBorderOptions, rrTopLeftBevel);
    end;
  end;
end;

procedure TBGRAJSONRoundRect.ProcessBorderBottomLeftStyle;
begin
  case FBorderBottomLeftStyle of
    TRoundRectangleStyle.rrsRounded:
    begin
      Exclude(FBorderOptions, rrBottomLeftSquare);
      Exclude(FBorderOptions, rrBottomLeftBevel);
    end;
    TRoundRectangleStyle.rrsBevel:
    begin
      Exclude(FBorderOptions, rrBottomLeftSquare);
      Include(FBorderOptions, rrBottomLeftBevel);
    end;
    TRoundRectangleStyle.rrsSquare:
    begin
      Include(FBorderOptions, rrBottomLeftSquare);
      Exclude(FBorderOptions, rrBottomLeftBevel);
    end;
  end;
end;

procedure TBGRAJSONRoundRect.ProcessBorderTopRightStyle;
begin
  case FBorderTopRightStyle of
    TRoundRectangleStyle.rrsRounded:
    begin
      Exclude(FBorderOptions, rrTopRightSquare);
      Exclude(FBorderOptions, rrTopRightBevel);
    end;
    TRoundRectangleStyle.rrsBevel:
    begin
      Exclude(FBorderOptions, rrTopRightSquare);
      Include(FBorderOptions, rrTopRightBevel);
    end;
    TRoundRectangleStyle.rrsSquare:
    begin
      Include(FBorderOptions, rrTopRightSquare);
      Exclude(FBorderOptions, rrTopRightBevel);
    end;
  end;
end;

procedure TBGRAJSONRoundRect.ProcessBorderBottomRightStyle;
begin
  case FBorderBottomRightStyle of
    TRoundRectangleStyle.rrsRounded:
    begin
      Exclude(FBorderOptions, rrBottomRightSquare);
      Exclude(FBorderOptions, rrBottomRightBevel);
    end;
    TRoundRectangleStyle.rrsBevel:
    begin
      Exclude(FBorderOptions, rrBottomRightSquare);
      Include(FBorderOptions, rrBottomRightBevel);
    end;
    TRoundRectangleStyle.rrsSquare:
    begin
      Include(FBorderOptions, rrBottomRightSquare);
      Exclude(FBorderOptions, rrBottomRightBevel);
    end;
  end;
end;

constructor TBGRAJSONRoundRect.Create;
begin
  FBackgroundColor := BGRAWhite;
  FBorderColor := BGRABlack;
  FWidth := 100;
  FHeight := 100;
  FWidthUnit := muPixels;
  FHeightUnit := muPixels;
  FBorderXRadius := 1;
  FBorderYRadius := 1;
  FBorderXRadiusUnit := muPixels;
  FBorderYRadiusUnit := muPixels;
  FBorderWidth := 1;
  FBorderTopLeftStyle := TRoundRectangleStyle.rrsSquare;
  FBorderTopRightStyle := TRoundRectangleStyle.rrsSquare;
  FBorderBottomLeftStyle := TRoundRectangleStyle.rrsSquare;
  FBorderBottomRightStyle := TRoundRectangleStyle.rrsSquare;
end;

procedure TBGRAJSONRoundRect.LoadFromFile(FileName: string);
var
  s: TStringList = nil;
begin
  if FileExists(FileName) then
  begin
    s := TStringList.Create;
    try
      s.LoadFromFile(FileName, TEncoding.UTF8);
      Style := s.Text;
    finally
      s.Free;
    end;
  end;
end;

procedure TBGRAJSONRoundRect.Draw(DestBitmap: TBGRABitmap; DestRect: TRect);
var
  halfBorderWidth: single;
  BorderXRealRadius, BorderYRealRadius: single;
  realWidth, realHeight: single;
begin
  try
    halfBorderWidth := max(BorderWidth / 2, 1);

    case FWidthUnit of
      muPercent: realWidth := Width * DestRect.Width / 100;
      else
        realWidth := Width;
    end;

    case FHeightUnit of
      muPercent: realHeight := Height * DestRect.Height / 100;
      else
        realHeight := Height;
    end;

    case FBorderXRadiusUnit of
      muPercent: BorderXRealRadius := BorderXRadius * realWidth / 100;
      else
        BorderXRealRadius := BorderXRadius;
    end;

    case FBorderXRadiusUnit of
      muPercent: BorderYRealRadius := BorderYRadius * realHeight / 100;
      else
        BorderYRealRadius := BorderYRadius;
    end;

    DestBitmap.RoundRectAntialias(DestRect.Left + halfBorderWidth,
      DestRect.Top + halfBorderWidth,
      DestRect.Left + realWidth - halfBorderWidth, DestRect.Top +
      realHeight - halfBorderWidth, BorderXRealRadius,
      BorderYRealRadius, BorderColor,
      BorderWidth, BackgroundColor, FBorderOptions);
  finally
  end;
end;

end.
