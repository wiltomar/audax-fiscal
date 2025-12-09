unit Model.ManifestoFiscal;

interface

uses
  Model.Base, System.DateUtils, APIService, System.SysUtils;

Type
  TEmpresa = class
  private
    fid: String;
    fnome: String;
    public
      property id: String   read fid   write fid;
      property nome: String read fnome write fnome;
  end;

Type
  TCorporacao = class
  private
    fid: String;
    fcodigo: String;
    fnome: String;
    public
      property id: String     read fid     write fid;
      property codigo: String read fcodigo write fcodigo;
      property nome: String   read fnome   write fnome;
  end;

Type
  TEstabelecimento = class
  private
    fid: String;
    fcodigo: String;
    fnome: String;
    public
      property id: String     read fid     write fid;
      property codigo: String read fcodigo write fcodigo;
      property nome: String   read fnome   write fnome;
  end;

Type
  TUsuario = class
  private
    fid: String;
    public
      property id: String read fid write fid;
  end;

Type
  TnotificacaoUsuario = class
  private
    fsituacao: Integer;
    fusuario: TUsuario;
    public
      constructor Create;
      destructor Destroy; override;
      property situacao: Integer read fsituacao write fsituacao;
      property usuario: TUsuario read fusuario  write fusuario;
  end;
  TnotificacaoUsuarios = TArray<TnotificacaoUsuario>;

Type
  TModeloDFE = class
  private
    fsituacao: Integer;
    ftipo: Integer;
    ftitulo: String;
    fmensagem: String;
    fseveridade: Integer;
    fsistema: boolean;
    freferencia: String;
    fempresa: TEmpresa;
    fcorporacao: TCorporacao;
    fusuario: TUsuario;
    freferenciaClasse: Integer;
    fmomento: String;
    fcompetencia: String;
    fnotificacaoUsuarios: TArray<TnotificacaoUsuario>;
    freferenciaId: String;
    public
      constructor Create;
      destructor Destroy; override;
      property situacao: Integer         read fsituacao         write fsituacao;
      property tipo: Integer             read ftipo             write ftipo;
      property severidade: Integer       read fseveridade       write fseveridade;
      property titulo: String            read ftitulo           write ftitulo;
      property mensagem: String          read fmensagem         write fmensagem;
      property competencia: String       read fcompetencia      write fcompetencia;
      property momento: String           read fmomento          write fmomento;
      property sistema: boolean          read fsistema          write fsistema;
      property referencia : String       read freferencia       write freferencia;
      property referenciaId: String      read freferenciaId     write freferenciaId;
      property referenciaClasse: Integer read freferenciaClasse write freferenciaClasse;
      property empresa: TEmpresa         read fempresa          write fempresa;
      property corporacao: TCorporacao   read fcorporacao       write fcorporacao;
      property usuario : TUsuario        read fusuario          write fusuario;
      property notificacaoUsuarios: TArray<TnotificacaoUsuario> read fnotificacaoUsuarios write fnotificacaoUsuarios;
  end;

Type
  TModeloConstel = class
  private
    fchaveDFe: String;
    fcnpjcpf: String;
    fvalorNF: Currency;
    femissao: TDateTime;
    public
      property cnpjcpf:  String    read fcnpjcpf  write fcnpjcpf;
      property chaveDFe: String    read fchaveDFe write fchaveDFe;
      property emissao:  TDateTime read femissao  write femissao;
      property valorNF:  Currency  read fvalorNF  write fvalorNF;
end;

type
  TCredencial = class
  private
    fid: string;
    fnome: string;
    femail: string;
    fimagem: string;
    fnavegador: string;
    ftoken: string;
    fcorporacao: TCorporacao;
    fempresa: TEmpresa;
    fvalidade: TDateTime;
    festabelecimento: TEstabelecimento;
  public
    property id: string read fid write fid;
    property nome: string            read fnome write fnome;
    property email: string           read femail write femail;
    property imagem: string          read fimagem write fimagem;
    property navegador: string       read fnavegador write fnavegador;
    property token: string           read ftoken write ftoken;
    property corporacao: TCorporacao read fcorporacao write fcorporacao;
    property empresa: TEmpresa       read fempresa write fempresa;
    property estabelecimento: TEstabelecimento read festabelecimento write festabelecimento;
    property validade: TDateTime     read fvalidade write fvalidade;
  public

  end;

Type
  Tdispositivo = class
  private
    fid: String;
    public
      property id: String read fid write fid;
  end;

Type
  TLogin = class
  private
    fusername: string;
    fpassword: string;
    fbrowser:  string;
    ftimezone: string;
    fdispositivo: TDispositivo;
  public
    property username: string read fusername write fusername;
    property password: string read fpassword write fpassword;
    property browser:  string read fbrowser  write fbrowser;
    property timezone: string read ftimezone write ftimezone;
    property dispositivo: Tdispositivo read fdispositivo write fdispositivo;
  end;

Type TTipoducumento = (CNPJ = 1, CPF = 2);

Type
  TManifestofiscal = class
  private
    fvalor: Currency;
    femitente: String;
    fexclusao: TDateTime;
    fedicao: TDateTime;
    fsituacao: SmallInt;
    finclusao: TDateTime;
    fchave: String;
    fatributos: string;
    fusuario: TUsuario;
    fcorporacao: TCorporacao;
    fempresa: TEmpresa;
    festabelecimento: TEstabelecimento;
    femissao: String;
    fambiente: SmallInt;
    fversaoXml: Double;
    fxml: String;
    fserie: integer;
    femitenteNome: String;
    femitenteDocumentoTipo: SmallInt;
    femitenteDocumentoNumero: String;
    fdestinatarioNome: String;
    fdestinatarioDocumentoTipo: SmallInt;
    fdestinatarioDocumentoNumero: String;
    fmodelo: String;
    fnumero: Integer;
    fdestinatarioCnpj: String;
    fdestinatarioCpf: String;
    femitenteCnpj: string;
    femitenteCpf: String;
  public
    constructor Create;
    destructor Destroy; override;
    function Login(Usuario:String;Senha: String; dispositivoId: String): String;
    property inclusao: TDateTime                 read finclusao        write finclusao;
    property edicao: TDateTime                   read fedicao          write fedicao;
    property exclusao: TDateTime                 read fexclusao        write fexclusao;
    property versaoXml : Double                  read fversaoXml       write fversaoXml ;
    property corporacao: TCorporacao             read fcorporacao      write fcorporacao;
    property empresa: TEmpresa                   read fempresa         write fempresa;
    property estabelecimento: TEstabelecimento   read festabelecimento write festabelecimento;
    property situacao: SmallInt                  read fsituacao        write fsituacao;
    property atributos: string                   read fatributos       write fatributos;
    property chave: String                       read fchave           write fchave;
    property emissao: String                     read femissao         write femissao;
    property emitente: String                    read femitente        write femitente;
    property valor: Currency                     read fvalor           write fvalor;
    property usuario: TUsuario                   read fusuario         write fusuario;
    property ambiente : SmallInt                 read fambiente        write fambiente;
    property xml: String                         read fxml             write fxml;
    property serie: integer                      read fserie           write fserie;
    property emitenteNome: String                read femitenteNome    write femitenteNome;
    property emitenteDocumentoTipo: SmallInt     read femitenteDocumentoTipo       write femitenteDocumentoTipo;
    property emitenteDocumentoNumero: String     read femitenteDocumentoNumero     write femitenteDocumentoNumero;
    property destinatarioNome: String            read fdestinatarioNome            write fdestinatarioNome;
    property destinatarioDocumentoTipo: SmallInt read fdestinatarioDocumentoTipo   write fdestinatarioDocumentoTipo;
    property destinatarioDocumentoNumero: String read fdestinatarioDocumentoNumero write fdestinatarioDocumentoNumero;
    property modelo: String                      read fmodelo           write fmodelo;
    property numero: Integer                     read fnumero           write fnumero;
    property destinatarioCnpj: String            read fdestinatarioCnpj write fdestinatarioCnpj;
    property destinatarioCpf: String             read fdestinatarioCpf  write fdestinatarioCpf;
    property emitenteCnpj: string                read femitenteCnpj     write femitenteCnpj;
    property emitenteCpf: String                 read femitenteCpf      write femitenteCpf;
  end;

implementation

constructor TModeloDFE.Create;
begin
  fempresa    := TEmpresa.Create();
  fCorporacao := TCorporacao.Create();
  fUsuario    := TUsuario.Create();
end;

destructor TModeloDFE.Destroy;
begin
  fempresa.Free;
  fCorporacao.Free;
  fUsuario.Free;
  inherited;
end;

constructor TnotificacaoUsuario.Create;
begin
  usuario := TUsuario.Create();
end;

destructor TnotificacaoUsuario.Destroy;
begin
  usuario.Free;
  inherited;
end;

constructor TManifestofiscal.Create;
begin
  fUsuario         := TUsuario.Create();
  fCorporacao      := TCorporacao.Create();
  fempresa         := TEmpresa.Create();
  festabelecimento := TEstabelecimento.Create();
end;

destructor TManifestofiscal.Destroy;
begin
  fUsuario.Free;
  fCorporacao.Free;
  fempresa.free;
  festabelecimento.Free;
  inherited;
end;

function TManifestofiscal.Login(Usuario:String;Senha: String; dispositivoId: String): String;
var
  credencial : TCredencial;
begin
  var
    Login := TLogin.Create();
  try
    Login.username := Usuario;
    Login.password := Senha;
    Login.timezone := TTimeZone.Local.Abbreviation;

    Login.dispositivo := Tdispositivo.create;
    login.dispositivo.id := dispositivoId;

    try
      credencial := InfoAPI().Post<TCredencial>('auth/login', Login);

      // FToken := Credencial.Token; // A ser usado nas próximas requisições externas
      Result := Credencial.empresa.nome;
    except
      on E: Exception do
      begin
        if Pos('401', E.Message) > 0 then // Verificar posteriormente
          E.Message := 'Login não autorizado, verifique suas credenciais';
        raise;
      end;
    end;
  finally
    Login.Free();
  end;
end;

end.
