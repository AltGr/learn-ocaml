This is Learn OCaml, a Web platform for learning the OCaml language,
featuring a Web toplevel, an exercise environment, and a directory of
lessons and tutorials.

This software is still under heavy development.

How to compile and try
----------------------

Note: you need a working ```opam``` environment with OCaml ```4.03.0```.

* Install the dependencies : ```sh install-opam-deps.sh```

* Compile the app : ```make```

You can customise the exercise repository and output directory using
make variables ```REPO_DIR``` and ```DEST_DIR```. By default,
the repository used is an included demo repository. The user
contributed exercises of the platform are in the git repository
```learn-ocaml-repository```. An exmple configuration is
```make REPO_DOR=../learn-ocaml-repository DEST_DIR=$HOME/public_html/learn-ocaml```.

* Put the resulting directory ```www/``` behind a Web server.

This last step is mandatory. If you try to open the ```index.html```
file directly from the local file system, it will fail for security
restrictions enforced by modern Web browsers. If you do not have a Web
server configured, you can probably use some other tool that is
already present on your machine. For instance, running ```python3 -m
http.server 9090``` or ```php -S localhost:9090``` in the ```www```
directory and pointing you browser to ```http://localhost:9090/```
should do the job.

License and copyright
---------------------

Unless explicitly written below or in the files themselves, the source
code for the app, images, static files, course content and exercises
and most of the are placed under the GNU Affero General Public License
version 3. This practically means that any instance of the app must
provide its source code to its users.
See [http://www.gnu.org/licenses/agpl-3.0.html].

Lightly modified third party components ACE and ppx_metaquot are
included, under their original licenses (respectively BSD and MIT).

The OCamlPro logo images are (c) OCamPro. Redistribution is permitted,
alteration requires prior written authorization by OCamlPro.

The OCaml / ocaml.org logo is released under the very liberal UNLICENSE.
See [https://github.com/ocaml/ocaml.org/blob/master/LICENSE.md].

The Inconsolata font is released under the Open Font License.
See [http://www.levien.com/type/myfonts/inconsolata.html].

The Biolinum font is licensed under the GNU General Public License with
a the 'Font-Exception'.
See [http://www.linuxlibertine.org].

The public instance of Learn OCaml uses the Fontin font instead of
Biolinum. This font is licensed under the exljbris Font Foundry Free
Font License Agreement, which, to our understanding, does not allow us
to redistribute it. See [http://www.exljbris.com/eula.html]. You will
optionally have to procure the files by yourself while building the
app. If not, the CSS provides a reasonable fallback font.

Contributions to this repository are placed under the BSD
license. This means that we can merge them with the same license as
the rest of the codebase, while you keep all the rights on your code.
And we will not have to bother you with any future license update.
See [https://opensource.org/licenses/BSD-3-Clause].

Authors and Acknowledgements
----------------------------

Learn OCaml is a software by OCamlPro.

 * The main authors are Benjamin Canou, Çağdaş Bozman and Grégoire Henry.

 * It builds on the previous experience of Try OCaml by Çağdaş Bozman.

 * We heavily use js_of_ocaml, so thanks to the Ocsigen team.

 * The text editing component is a cutomized version of ACE.

 * We also include a derivative of ppx_metaquot by Alain Frisch.