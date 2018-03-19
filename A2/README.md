<center>

# Department of Computer Science and Engineering

## Programming Languages

### II Semester 2017-18

#### Instructor: [S. Arun-Kumar]

##### Assignment 2

###### A Markdown translator for HTML text + HTML tables from csv format
</center>

We need to do the following. Create a markdown+table *functional* program in SML
using the functionalities given in <tt>fileIO.sml</tt>.

10. This HTML file <tt>mdtab.txt.html</tt> was produced by running
    [markdown] [jg] on the text file <tt>mdtab.txt</tt>.
    Use the text file to test your own program.

8. To know about markdown go to the [markdown syntax][jg]
   page maintained by John Gruber. Many of the simplifications listed
   below also refer to Gruber's document.

4. Recognize and leave all inline HTML tags unchanged.

5. **Headings.** Do not implement the underlining feature of
   markdown. Implement only the headings defined by hashes.
   Hence you must implement the 6 levels of headings which
   are the levels shown in
   the centered lines at the beginning of this document. The
   number of hashes at the beginning of the line indicates the
   heading level.

3. Implement the following for pure text elements --
   Use of double asterisks for **bolding** and single asterisks for
   *italics*.
   There is nothing in markdown for underlining -- under-scores are
   used for italics and bolding as an alternative to
   asterisks. Instead implement underlining as follows.
   Example: "\_underlined\_text\_" produces <u>underlined text</u>).

9. **Horizontal Ruling**. Implement only the code corresponding to
   the use of at least 3 consecutive hypens (---) to produce
   a horizontal line.

2. **Block quotes.** Implement only the email-style block-quoting
   (i.e. every line in a block quote should begin with >,
   otherwise it is not a block quote) along with the nested
   block-quoting.

6. **Links.** Implement the direct href produced by the
   using the "\[text\]\(http://url/of/the/link\)" syntax. Also implement the
   automatic links feature e.g.

   	     <http://www.cse.iitd.ac.in/~sak>

   renders as <http://www.cse.iitd.ac.in/~sak> which
   provides the URL and the link to the URL.

7. **Lists.** Implement

   - *ordered lists* -- as given by [John Gruber] [jg]. Pay particular
     attention to his 1986 example relating to escape sequencing the
     "." in case 1986 comes at the beginning of a line.
   - *unordered lists* -- implement only the hyphen (as done here).

1. Implement tables from csv format. For example the following is

   	     <<
   	     1|2|3|4|9
   	     5|6|7|8|10
   	     11|12|13|14|15
   	     >>

   a possible syntax for an HTML table that looks like

<CENTER><TABLE border="1">
  <TR><TD>1</TD><TD>2</TD><TD>3</TD><TD>4</TD><TD>9</TD></TR>
  <TR><TD>5</TD><TD>6</TD><TD>7</TD><TD>8</TD><TD>10</TD></TR>
  <TR><TD>11</TD><TD>12</TD><TD>13</TD><TD>14</TD><TD>15</TD></TR>
</TABLE></CENTER>

0. Since this is as clear as possible and has already been discussed
   in class, I will entertain no individual doubts on any forum. If you
   still have some doubts, <font color=red>make reasonable design
   decisions and explicitly state them as comments at the top of your
   submission file.

[jg]: https://daringfireball.net/projects/markdown/syntax
