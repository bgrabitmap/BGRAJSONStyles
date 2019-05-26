unit umain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls,
  BGRAGraphicControl, BGRABitmap, BGRABitmapTypes, BGRAJSONStyles,
  FileUtil;

type

  { TForm1 }

  TForm1 = class(TForm)
    BGRAGraphicControl1: TBGRAGraphicControl;
    ListBox1: TListBox;
    procedure BGRAGraphicControl1Redraw(Sender: TObject; Bitmap: TBGRABitmap);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure ListBox1SelectionChange(Sender: TObject; User: boolean);
  private
    style: TBGRAJSONRoundRect;
  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.BGRAGraphicControl1Redraw(Sender: TObject; Bitmap: TBGRABitmap
  );
begin
  Bitmap.FillTransparent;
  Bitmap.DrawCheckers(Rect(0, 0, Bitmap.Width, Bitmap.Height), BGRAWhite, BGRA(220, 220, 220));
  style.Draw(Bitmap, Rect(0, 0, Bitmap.Width, Bitmap.Height));
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  style := TBGRAJSONRoundRect.Create;
  FindAllFiles(ListBox1.Items, Application.Location + PathDelim + 'styles' + PathDelim, '*.json', False);
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  style.Free;
end;

procedure TForm1.ListBox1SelectionChange(Sender: TObject; User: boolean);
begin
  style.LoadFromFile(ListBox1.Items[ListBox1.ItemIndex]);
  BGRAGraphicControl1.DiscardBitmap;
end;

end.

