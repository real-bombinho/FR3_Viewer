program Report;

uses
  System.StartUpCopy,
  FMX.Forms,
  Main in 'Main.pas' {Form1},
  DPPreview in 'DPPreview.pas' {Preview: TFrame},
  DPControls in 'DPControls.pas',
  DPFile in 'DPFile.pas',
  DPColors in 'DPColors.pas',
  ColourTool in 'ColourTool.pas' {Tool};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.CreateForm(TTool, Tool);
  Application.Run;
end.
