unit DPFile;

interface

uses XML.XMLDoc, XML.XMLintf;

type

FR3Document = class(TXMLDocument)
  private
    FRoot: String;
  public
    property Root: String read FRoot;
    function LoadFromFile(FileName: string): boolean;


end;

FR3Node = interface(IXMLNode)

end;

implementation

{ FR3Document }

function FR3Document.LoadFromFile(FileName: string): boolean;
var n: IXMLNode;
begin
  inherited LoadFromFile(FileName);
  //FRoot := DocumentElement.NodeName;
end;

end.
