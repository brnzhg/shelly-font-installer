# Shelly Font Installer
## low in functionality, high in dependencies

I pretty much just use this to install source-code-pro for dev environments

Project for learning Haskell libs...
* shelly
* optparse-applicative
* Text.PrettyPrint.ANSI.Leijen
* mtl
* MonadCatch
* basic lenses

## Usage
<pre>
shelly font installer - a dumb font installer

Usage: shelly_font_installer.hs COMMAND [--localpath LOCAL FONT PATH]
                                ([--syspath SYSTEM FONT PATH] | [--nosyspath])
  installs fonts from the shell, with way too many Haskell dependencies

Available options:
  --localpath LOCAL FONT PATH
                           Local font
                           path (default: FilePath "/home/bzhang/.local/share/fonts")
  --syspath SYSTEM FONT PATH
                           System font
                           path (default: FilePath "/usr/local/share/fonts")
  --nosyspath              No system font path
  -h,--help                Show this help text

Available commands:
  status                   
  install        
</pre>
