# Heatmapper

Heatmapper is a simple command line utility to output github style
heatmaps on the command line.

It's specifically focused on *just* the heatmap, and intended to be leveraged by other programs. Either via the command line OR via your favorite language's ability to leverage C libraries (see below).

If you want to generate a command line heatmap *with* your GitHub information I recommend you
check out these two projects

-   [Heatwave](https://github.com/james-stoup/heatwave) by [James
    Stoup](https://github.com/james-stoup) (Python)
-   [Git Stats](https://github.com/IonicaBizau/git-stats) by [Ionică
    Bizău](https://github.com/IonicaBizau) (JavaScript)

**Example**  
![example output](../readme_images/images/style_examples.png)

## Installation

Heatmapper is written in [Chicken Scheme](https://wiki.call-cc.org)

You can install that with `brew install chicken-scheme` if
you're on macOS or Linux, otherwise follow the instructions on their
site.

Once you've gotten that installed:

    cd src
    # install the required libraries
    ./install_chicken_eggs.sh
    ./build.sh

That's it. You should now have a `heatmapper` executable in
the same directory. 

There are a couple test files in misc:
- `numbers.txt`
- `random_numbers.txt`

Try running `cat ../misc/random_numbers.txt | ./heatmapper` just to make sure everything's working.
If everything looks good, copy that somewhere in your PATH and you're good to go.


## Usage

Just pipe some data into it and it\'ll generate at heatmap 80 columns
wide and 7 rows tall.

``` bogus
❯ ./heatmapper --help
Usage: piped data | heatmapper heatmapper [options...]
       Supported color schemes: github, wistia

 -c, --columns=ARG        max number of columns [default: 80]
 -r, --rows=ARG           max number of rows [default: 7]
 -s, --scheme=ARG         color scheme [default: github]
 -h, --help               Display this text

Report bugs to https://github.com/masukomi/heatmapper/issues
```


## Usage notes

The data fills from from the top left corner down, then moves one column
to the right, and fills down again. It stops when it runs out of data or
hits the maximum number of columns. Unfinished rows will be filled out with
zero values.


A good way to have it always use your current terminal width (in bash / zsh) is to use `tput` as follows:

``` bash
cat numbers.txt | heatmapper --columns $(tput cols)
```

### Pro-tip

If you want to use this in something that generates HTML you could use [oho](https://github.com/masukomi/oho) to convert the output to HTML. 

## Adding a Color Scheme

Adding a color scheme is pretty easy, even if you don't know Chicken
Scheme. You'll need a palette of 5 colors. Get their RGB values, then
open up the `src/heatmap.scm` file.

The first `define` in that file is setting up the color
schemes. Just replicate what's in the existing ones, change the color
numbers, and give it a new name. Note that the colors are ordered from
lightest to darkest.

The color name comments are entirely optional, but nice to have. :D

## There's C code too

Chicken Scheme compiles to C.

You can actually generate the C source files and headers for use in
other projects. I've specifically designed this so that you can have
*just* the core library without the command line handling stuff.

The file you want is `src/heatmap.scm`

Follow [the instructions
here](https://wiki.call-cc.org/man/5/Deployment#distributing-compiled-c-files)
to generate the C files and headers.

### Oh, and ruby
Before I ported this to Chicken Scheme I prototyped the idea with a quick and dirty ruby script. You can find it in `misc/ruby_prototype.rb`

## Contact & Contribution

If you do use this in something, please [Let me know on
Mastodon](https://connectified.com/@masukomi).

If you've got an issue, please [file a ticket here on
GitHub](https://github.com/masukomi/heatmapper/issues).

## License
This code is distributed under the AGPL license. See the `LICENSE` file for more details.

If you need to incorporate this into a commercial product I'm happy to give you a difference license in exchange for money. ;) 
