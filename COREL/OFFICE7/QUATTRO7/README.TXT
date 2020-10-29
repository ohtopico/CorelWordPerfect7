LEIAME.TXT
Notas de Atualização do Corel Quattro Pro (Rev. 28/06/96)
=========================================================

Este arquivo contém informações importantes que substituem as contidas no 
Guia do Usuário. Leia com atenção.

Sumário
-------
  1. Tópicos da instalação
  2. Usando o ModeloRápido
  3. Usando as definições IDAPI existentes
  4. Documentação do Corel Quattro Pro 
  5. Usando os demos do Corel Quattro Pro 
  6. Conversões PerfectScript
  7. Designer de Diálogo
  8. Formatos de Gráficos
  9. @Funções
 10. Arquivos de demonstração
 11. Publicar em Corel Barista
 12. Importar texto/Dividir

1. Tópicos da instalação
------------------------
Ao instalar o Corel Quattro Pro em uma estação de trabalho o IDAPI.CFG 
permanece no diretório do servidor \COREL\OFFICE7 como só leitura. Para 
instalar o arquivo, o administrador da rede pode remover o atributo de só 
leitura do arquivo, copiar o arquivo em cada estação de trabalho e, em 
seguida, o usuário estabelece a definição do Paradox NET DIR usando o 
Utilitário de Configuração IDAPI.

2. Usando o ModeloRápido
------------------------
Para configurar o Corel Quattro Pro para usar o ModeloRápido, clique em 
Editar>Preferências>Opções de Arquivo, defina o ModeloRápido para 
COREL\OFFICE7\TEMPLATE e em seguida clique em OK.

3. Usando as definições IDAPI existentes
----------------------------------------
Para incorporar os aliases IDAPI existentes à versão 32-bit 
do IDAPI usado pelo Corel Quattro Pro, use o Utilitário de Configuração 
IDAPI para intercalar o arquivo IDAPI.CFG existente com o novo IDAPI.CFG de 
32 bits. Consulte a seção 1 deste arquivo para obter maiores informações 
sobre a instalação do IDAPI.CFG durante uma instalação de rede.

4. Documentação do Corel Quattro Pro 
------------------------------------
O Guia do Usuário completo do Corel Quattro Pro está disponível como um 
documento Envoy no CD-ROM do Corel Office Professional 7 ou Corel 
WordPerfect Suite 7. Abra o arquivo \COREL\OFFICE7\SHARED\HELP7\QP7US.EVY.

5. Usando os demos do Corel Quattro Pro 
---------------------------------------
Para melhores resultados na visualização dos Demos do Corel Quattro Pro, 
use um controlador de tela de 256 cores no Windows 95.

6. Conversões PerfectScript
---------------------------
Se você está convertendo as macros do Corel Quattro Pro em macros do 
PerfectScript, os seguintes comandos de macro do Corel Quattro Pro devem 
ser substituídos pelos comandos correspondentes do PerfectScript:

        Corel Quattro Pro     PerfectScript
        -----------------------------------------
        {ABS}                 ReferênciaAbsoluta()
        {ASSIGN}              AtribuirValor()

7. Designer de Diálogo
----------------------
Existem três controles de diálogo novos no Corel Quattro Pro 7 
não indicados nas versões 6.x anteriores. Se você usar o controle de 
Tab, controle de Cor ou controles de Seleção de Arquivo no Corel Quattro 
Pro 7 e gravar o arquivo como uma versão 6.x (.WB2), estes controles não 
serão gravados.

8. Formatos de Gráficos
-----------------------
O Corel Quattro Pro exporta os seguintes formatos de arquivo de gráfico 
em 256 cores ou 16 cores apenas: CompuServe Bitmap (GIF), Paintbrush (PCX) 
e TIFF Bitmap (TIF). O Corel Quattro Pro também exporta os seguintes 
formatos de arquivo de gráfico em 256-, 16-, e resoluções mais altas: 
Window Bitmap (BMP), Encapsulated Postscript (EPS) e Computer Graphic 
Metafile (CGM). Se o seu sistema tiver uma resolução mais alta do que 256 
ou 16 cores, as opções de " exportar para arquivo " para GIF, PCX. e TIF 
não estarão disponíveis.

O Corel Quattro Pro importa os seguintes formatos de arquivo de gráfico:
CGM, CLP, EPS, GIF, JPG, PCX, PIC, TIF, e WPG.

9. @Funções
-----------
@DATA agora suporta ANOS > 1900.
Exemplo: @DATA(1992,2,2) é igual a @DATA(92,2,2)

10. Arquivos de demonstração
----------------------------
Existem 12 demos para o Corel Quattro Pro localizados no Disco de 
Biblioteca da Corel. Para executar um demo, faça o seguinte:
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
O Corel Quattro Pro agora suporta a publicação em Corel Barista, a nova 
tecnologia da Corel de publicação da Internet com base em Java. Para 
permitir a publicação em Corel Barista, o arquivo QPJV70.DLL deve estar 
localizado no mesmo diretório do arquivo de programa QPW.EXE do Corel 
Quattro Pro. Em uma instalação padrão, o QPJV70.DLL deve ser copiado no 
diretório Corel\Office7\Quattro 7.

Um caderno ou um bloco de células de dados pode ser publicado em Corel 
Barista com a seleção do item de menu Arquivo | Publicar em | Corel Barista. 
Este item de menu apenas aparece se o arquivo QPJV70.DLL está no mesmo 
diretório do arquivo de programa QPW.EXE.

A versão atual do Corel Quattro Pro tem algumas limitações sobre o que 
pode ser publicado em Corel Barista. Os cadernos que contêm texto de 
cores sólidas funcionam melhor. Os itens que atualmente não publicam em 
Corel Barista são:

  - Gráficos flutuantes e mapas
  - Todas as anotações gráficas
  - Estampas e manchas

Você pode obter a última versão do arquivo QPJV70.DLL através da home page 
do Corel Quattro Pro na Internet em http://www.corel.com/products/wordperfect/cqp7.


12. Importar texto/Dividir
--------------------------
O novo QuickColumns Expert oferece flexibilidade e eficácia na divisão e 
composição do texto a partir de um bloco ou arquivo.

Para manter a compatibilidade, a tecnologia de divisão encontrada em 
versões anteriores do Corel Quattro Pro ainda está disponível a partir do 
idioma de macro como {FileImport?} e {Parse?}. É fácil adicionar um botão 
a Barra de Ferramentas que iniciará os comandos antigos Dados|Dividir e 
Dados|Importar Texto.

Use os seguintes passos para adicionar um botão a Barra de Ferramentas:

    1. Clique com o botão direito do mouse na Barra de Ferramentas e escolha Editar...
    2. Selecione a aba Macro
    3. Para Importar Texto, adicione o seguinte texto ao campo de edição 
       Inserir Macro: {FileImport?}
       Para Dividir, adicione o seguinte texto ao campo de edição
       Inserir Macro: {Parse?}
    4. Clique em Fechar.
