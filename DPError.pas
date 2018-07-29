unit DPError;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Edit,
  FMX.StdCtrls, FMX.Controls.Presentation;

type
  TError = class(TForm)
    Button1: TButton;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Edit1: TEdit;
    Edit2: TEdit;
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    Verbose: boolean;
    procedure Show(const Name: string; const Value: string);
  end;

var
  Error: TError;

implementation

{$R *.fmx}

{ TError }

procedure TError.Button1Click(Sender: TObject);
begin
  Close;
end;

procedure TError.FormCreate(Sender: TObject);
begin
  Verbose := true;
end;

procedure TError.Show(const Name, Value: string);
begin
  Error.Edit1.Text := Name;
  Error.Edit2.Text := value;
  if Verbose then Error.ShowModal;
end;

end.
