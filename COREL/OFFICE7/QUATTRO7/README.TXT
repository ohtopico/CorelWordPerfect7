LEIAME.TXT
Notas de Atualiza��o do Corel Quattro Pro (Rev. 28/06/96)
=========================================================

Este arquivo cont�m informa��es importantes que substituem as contidas no 
Guia do Usu�rio. Leia com aten��o.

Sum�rio
-------
  1. T�picos da instala��o
  2. Usando o ModeloR�pido
  3. Usando as defini��es IDAPI existentes
  4. Documenta��o do Corel Quattro Pro 
  5. Usando os demos do Corel Quattro Pro 
  6. Convers�es PerfectScript
  7. Designer de Di�logo
  8. Formatos de Gr�ficos
  9. @Fun��es
 10. Arquivos de demonstra��o
 11. Publicar em Corel Barista
 12. Importar texto/Dividir

1. T�picos da instala��o
------------------------
Ao instalar o Corel Quattro Pro em uma esta��o de trabalho o IDAPI.CFG 
permanece no diret�rio do servidor \COREL\OFFICE7 como s� leitura. Para 
instalar o arquivo, o administrador da rede pode remover o atributo de s� 
leitura do arquivo, copiar o arquivo em cada esta��o de trabalho e, em 
seguida, o usu�rio estabelece a defini��o do Paradox NET DIR usando o 
Utilit�rio de Configura��o IDAPI.

2. Usando o ModeloR�pido
------------------------
Para configurar o Corel Quattro Pro para usar o ModeloR�pido, clique em 
Editar>Prefer�ncias>Op��es de Arquivo, defina o ModeloR�pido para 
COREL\OFFICE7\TEMPLATE e em seguida clique em OK.

3. Usando as defini��es IDAPI existentes
----------------------------------------
Para incorporar os aliases IDAPI existentes � vers�o 32-bit 
do IDAPI usado pelo Corel Quattro Pro, use o Utilit�rio de Configura��o 
IDAPI para intercalar o arquivo IDAPI.CFG existente com o novo IDAPI.CFG de 
32 bits. Consulte a se��o 1 deste arquivo para obter maiores informa��es 
sobre a instala��o do IDAPI.CFG durante uma instala��o de rede.

4. Documenta��o do Corel Quattro Pro 
------------------------------------
O Guia do Usu�rio completo do Corel Quattro Pro est� dispon�vel como um 
documento Envoy no CD-ROM do Corel Office Professional 7 ou Corel 
WordPerfect Suite 7. Abra o arquivo \COREL\OFFICE7\SHARED\HELP7\QP7US.EVY.

5. Usando os demos do Corel Quattro Pro 
---------------------------------------
Para melhores resultados na visualiza��o dos Demos do Corel Quattro Pro, 
use um controlador de tela de 256 cores no Windows 95.

6. Convers�es PerfectScript
---------------------------
Se voc� est� convertendo as macros do Corel Quattro Pro em macros do 
PerfectScript, os seguintes comandos de macro do Corel Quattro Pro devem 
ser substitu�dos pelos comandos correspondentes do PerfectScript:

        Corel Quattro Pro     PerfectScript
        -----------------------------------------
        {ABS}                 Refer�nciaAbsoluta()
        {ASSIGN}              AtribuirValor()

7. Designer de Di�logo
----------------------
Existem tr�s controles de di�logo novos no Corel Quattro Pro 7 
n�o indicados nas vers�es 6.x anteriores. Se voc� usar o controle de 
Tab, controle de Cor ou controles de Sele��o de Arquivo no Corel Quattro 
Pro 7 e gravar o arquivo como uma vers�o 6.x (.WB2), estes controles n�o 
ser�o gravados.

8. Formatos de Gr�ficos
-----------------------
O Corel Quattro Pro exporta os seguintes formatos de arquivo de gr�fico 
em 256 cores ou 16 cores apenas: CompuServe Bitmap (GIF), Paintbrush (PCX) 
e TIFF Bitmap (TIF). O Corel Quattro Pro tamb�m exporta os seguintes 
formatos de arquivo de gr�fico em 256-, 16-, e resolu��es mais altas: 
Window Bitmap (BMP), Encapsulated Postscript (EPS) e Computer Graphic 
Metafile (CGM). Se o seu sistema tiver uma resolu��o mais alta do que 256 
ou 16 cores, as op��es de " exportar para arquivo " para GIF, PCX. e TIF 
n�o estar�o dispon�veis.

O Corel Quattro Pro importa os seguintes formatos de arquivo de gr�fico:
CGM, CLP, EPS, GIF, JPG, PCX, PIC, TIF, e WPG.

9. @Fun��es
-----------
@DATA agora suporta ANOS > 1900.
Exemplo: @DATA(1992,2,2) � igual a @DATA(92,2,2)

10. Arquivos de demonstra��o
----------------------------
Existem 12 demos para o Corel Quattro Pro localizados no Disco de 
Biblioteca da Corel. Para executar um demo, fa�a o seguinte:
   1. Insira o Disco de Biblioteca da Corel na unidade do CD-ROM.
   2. Abra o Windows Explorer.
   3. Abra a pasta DEMOS (por exemplo, D:\DEMOS).
   4. Clique duas vezes em qualquer um dos seguintes demos do 
      Corel Quattro Pro:

      FORMCMP.AVI
      LINEDRAW.AVI
      QKFILL.AVI
      QKTEMP.AVI
      SPDFMT.AVI

      CHART.EXE
      DATA_MAP.EXE
      FORMCOPY.EXE
      FORMULA.EXE
      FUNCTION.EXE
      NOTEBOOK.EXE
      SPRSHEET.EXE

11. Publicar para Corel Barista
-------------------------------
O Corel Quattro Pro agora suporta a publica��o em Corel Barista, a nova 
tecnologia da Corel de publica��o da Internet com base em Java. Para 
permitir a publica��o em Corel Barista, o arquivo QPJV70.DLL deve estar 
localizado no mesmo diret�rio do arquivo de programa QPW.EXE do Corel 
Quattro Pro. Em uma instala��o padr�o, o QPJV70.DLL deve ser copiado no 
diret�rio Corel\Office7\Quattro 7.

Um caderno ou um bloco de c�lulas de dados pode ser publicado em Corel 
Barista com a sele��o do item de menu Arquivo | Publicar em | Corel Barista. 
Este item de menu apenas aparece se o arquivo QPJV70.DLL est� no mesmo 
diret�rio do arquivo de programa QPW.EXE.

A vers�o atual do Corel Quattro Pro tem algumas limita��es sobre o que 
pode ser publicado em Corel Barista. Os cadernos que cont�m texto de 
cores s�lidas funcionam melhor. Os itens que atualmente n�o publicam em 
Corel Barista s�o:

  - Gr�ficos flutuantes e mapas
  - Todas as anota��es gr�ficas
  - Estampas e manchas

Voc� pode obter a �ltima vers�o do arquivo QPJV70.DLL atrav�s da home page 
do Corel Quattro Pro na Internet em http://www.corel.com/products/wordperfect/cqp7.


12. Importar texto/Dividir
--------------------------
O novo QuickColumns Expert oferece flexibilidade e efic�cia na divis�o e 
composi��o do texto a partir de um bloco ou arquivo.

Para manter a compatibilidade, a tecnologia de divis�o encontrada em 
vers�es anteriores do Corel Quattro Pro ainda est� dispon�vel a partir do 
idioma de macro como {FileImport?} e {Parse?}. � f�cil adicionar um bot�o 
a Barra de Ferramentas que iniciar� os comandos antigos Dados|Dividir e 
Dados|Importar Texto.

Use os seguintes passos para adicionar um bot�o a Barra de Ferramentas:

    1. Clique com o bot�o direito do mouse na Barra de Ferramentas e escolha Editar...
    2. Selecione a aba Macro
    3. Para Importar Texto, adicione o seguinte texto ao campo de edi��o 
       Inserir Macro: {FileImport?}
       Para Dividir, adicione o seguinte texto ao campo de edi��o
       Inserir Macro: {Parse?}
    4. Clique em Fechar.
