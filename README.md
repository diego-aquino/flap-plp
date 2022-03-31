<h1 align="center">
  <img src=".github/assets/icon.png" width="72px" align="center">&nbsp;&nbsp;
  FlapPLP
</h1>

- [Projeto funcional (Haskell)](#projeto-funcional-haskell)
  - [Instalação](#instalação)
    - [Requisitos](#requisitos)
  - [Executando localmente](#executando-localmente)
- [Projeto funcional (Prolog)](#projeto-funcional-prolog)
  - [Instalação](#instalação-1)
    - [Requisitos](#requisitos-1)
  - [Executando localmente](#executando-localmente-1)
- [Arquitetura](#arquitetura)
- [Autores](#autores)

# Projeto funcional (Haskell)

## Instalação

### Requisitos

Para utilizar a versão funcional desse projeto, escrita em [Haskell](https://www.haskell.org/), é preciso ter as seguintes ferramentas instaladas:

- [Git](https://git-scm.com/)
- [Cabal](https://www.haskell.org/cabal/)

## Executando localmente

1. Clonar repositório

   ```bash
   git clone https://github.com/diego-aquino/flap-plp.git
   cd flap-plp/Haskell
   ```

2. Configurar o Cabal e instalar as dependências

   ```bash
   cabal update
   cabal install cabal-install
   cabal build
   ```

3. Executar o projeto

   ```bash
   cabal run
   ```

# Projeto funcional (Prolog)

## Instalação

### Requisitos

Para utilizar a versão funcional desse projeto, escrita em [Prolog](https://www.swi-prolog.org/), é preciso ter as seguintes ferramentas instaladas:

- [Git](https://git-scm.com/)
- [SWI Prolog](https://www.swi-prolog.org/download/stable)

## Executando localmente

É necessário ter um ambiente **Unix** para executar o projeto funcional, pois algumas funcionalidades avançadas do terminal não estão disponíveis em Prolog para Windows.

1. Clonar repositório

   ```bash
   git clone https://github.com/diego-aquino/flap-plp.git
   cd flap-plp/Prolog
   ```

2. Executar o projeto

   ```bash
   swipl -q -f Main.pl
   ```

   Depois de entrar no prompt interativo, chame a regra `main`:

   ```bash
   ?- main.
   ```

Para uma melhor experiência, recomenda-se ajustar o tamanho do terminal para as seguintes dimensões:

- Altura: ~30 caracteres
- Largura: ~130 caracteres

# Arquitetura

Na implementação, buscamos definir uma arquitetura de entidades e módulos, de modo a manter coesão e facilitar mudanças futuras. Abaixo, estão os principais módulos criados e os seus relacionamentos:

- **GameController**: controlador central do jogo, dedicado a definir as regras de negócio, lidar com os inputs do usuário e coordenar outras entidades.

- **GameState**: responsável por armazenar o estado do jogo, incluindo o tipo de tela atual (em jogo, pausado ou game over), o pássaro, os canos, a pontuação atual e a pontuação máxima atingida.

- **GameScreen**: dedicado fazer a renderização dos frames no terminal, de acordo com o estado atual do jogo (game state).

- **Bird**: representa o jogador, que pode voar e colidir com canos ou com as bordas da tela, sempre sofrendo a ação da gravidade.

- **Pipe** e **PipeGroup**: representam os obstáculos enfrentados pelo jogador.

- **Area** e **Line**: representam a área ocupada por uma entidade (bird ou pipes), de acordo com as suas coordenadas e a sua representação em string, sendo utilizados no processamento das colisões.

- **Terminal**: dedicado a lidar com o terminal, como obter os inputs do usuário, fornecer as dimensões da tela e movimentar o cursor.

- **LocalStorage**: responsável por ler e salvar dados localmente, sendo utilizado para manter a pontuação máxima entre as execuções do programa.

# Autores

- Diego Aquino ([diego-aquino](https://github.com/diego-aquino))
- Augusto Nunes ([augustonz](https://github.com/augustonz))
- Murilo Gruppi ([Murilo-Gruppi](https://github.com/Murilo-Gruppi))
- Eduardo Gabriel ([edugabriel12](https://github.com/edugabriel12))
