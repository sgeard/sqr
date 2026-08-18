# tclcalc.py — LibreOffice Calc macro host for sqr_calc.tcl.
#
# This file is the generic half of the two-part Calc client: it knows sheets
# and cells (pyuno) and nothing about sqr.  Each macro invocation creates a
# fresh embedded Tcl interpreter (tkinter.Tcl() — displayless, ~1 ms),
# registers the six servant commands into it, sources sqr_calc.tcl (kept
# alongside this file) and calls sqr::main.  All application logic lives in
# the Tcl script; this file should never need to change with it.
#
# Servant contract (see the header of sqr_calc.tcl for the authoritative
# description): sheet / used / getcells / putcells / clearcells / msg,
# 1-based coordinates, cells tagged (V number | S text | N empty).
# A Calc cell holding an empty string is reported as N: blanks are never
# significant.
#
# Install: copy this file and sqr_calc.tcl to
#   ~/.config/libreoffice/4/user/Scripts/python/
# (make install-calc does exactly that), then bind the sqr_pull / sqr_push
# macros to buttons or shortcuts in Calc.  Requires the system Python's
# tkinter (package python3-tkinter) — LibreOffice's scripting Python is the
# system Python on this distribution.

import os
import tkinter
import uno

from com.sun.star.awt.MessageBoxType import INFOBOX
from com.sun.star.awt.MessageBoxButtons import BUTTONS_OK

# css.sheet.CellFlags: VALUE | DATETIME | STRING | FORMULA
_CLEAR_FLAGS = 1 | 2 | 4 | 16


class _Servant:
    """The spreadsheet surface offered to the Tcl director."""

    def __init__(self, doc, interp):
        self.doc = doc
        self.interp = interp
        self.sheet_obj = None

    # -- helpers ----------------------------------------------------------

    def _used_address(self):
        cur = self.sheet_obj.createCursor()
        cur.gotoStartOfUsedArea(False)
        cur.gotoEndOfUsedArea(True)
        return cur.RangeAddress          # 0-based rows/columns

    def _range(self, r1, c1, r2, c2):
        return self.sheet_obj.getCellRangeByPosition(c1 - 1, r1 - 1,
                                                     c2 - 1, r2 - 1)

    # -- servant commands (registered under these names) ------------------

    def sheet(self, name):
        sheets = self.doc.Sheets
        created = 0
        if not sheets.hasByName(name):
            sheets.insertNewByName(name, sheets.Count)
            created = 1
        self.sheet_obj = sheets.getByName(name)
        return created

    def used(self):
        ra = self._used_address()
        return (ra.StartRow + 1, ra.StartColumn + 1,
                ra.EndRow + 1, ra.EndColumn + 1)

    def getcells(self, r1, c1, r2, c2):
        rng = self._range(int(r1), int(c1), int(r2), int(c2))
        out = []
        for row in rng.getDataArray():
            for v in row:
                if isinstance(v, str):
                    out.append(('N',) if v == '' else ('S', v))
                else:
                    out.append(('V', v))
        return tuple(out)

    def putcells(self, r1, c1, nr, nc, block):
        r1, c1, nr, nc = int(r1), int(c1), int(nr), int(nc)
        cells = self.interp.tk.splitlist(block)
        if len(cells) != nr * nc:
            raise ValueError('putcells: %d cells for a %dx%d block'
                             % (len(cells), nr, nc))
        rows = []
        k = 0
        for _ in range(nr):
            row = []
            for _ in range(nc):
                cell = self.interp.tk.splitlist(cells[k])
                k += 1
                tag = cell[0]
                if tag == 'N':
                    row.append('')
                elif tag == 'S':
                    row.append(cell[1])
                elif tag == 'V':
                    row.append(float(cell[1]))
                else:
                    raise ValueError('putcells: bad cell tag %r' % (tag,))
            rows.append(tuple(row))
        rng = self._range(r1, c1, r1 + nr - 1, c1 + nc - 1)
        rng.setDataArray(tuple(rows))
        return ''

    def clearcells(self):
        ra = self._used_address()
        rng = self.sheet_obj.getCellRangeByPosition(ra.StartColumn, ra.StartRow,
                                                    ra.EndColumn, ra.EndRow)
        rng.clearContents(_CLEAR_FLAGS)
        return ''

    def msg(self, text):
        parent = self.doc.CurrentController.Frame.ContainerWindow
        box = parent.Toolkit.createMessageBox(parent, INFOBOX, BUTTONS_OK,
                                              'sqr', text)
        box.execute()
        return ''


def _script_path():
    """sqr_calc.tcl lives beside this file (the script provider may report
    __file__ as a file:// URL)."""
    here = __file__
    if here.startswith('file://'):
        here = uno.fileUrlToSystemPath(here)
    return os.path.join(os.path.dirname(os.path.abspath(here)), 'sqr_calc.tcl')


def _run(verb):
    doc = XSCRIPTCONTEXT.getDocument()          # noqa: F821 (injected by LO)
    interp = tkinter.Tcl()
    servant = _Servant(doc, interp)
    for name in ('sheet', 'used', 'getcells', 'putcells', 'clearcells', 'msg'):
        interp.tk.createcommand(name, getattr(servant, name))
    try:
        interp.tk.call('source', _script_path())
        interp.tk.call('sqr::main', verb)
    except tkinter.TclError as e:
        try:
            servant.msg('sqr error: %s' % e)
        except Exception:
            pass


def sqr_pull(*args):
    """Pull the configured table into its data sheet."""
    _run('pull')


def sqr_push(*args):
    """Push the data sheet back to the configured table."""
    _run('push')


g_exportedScripts = (sqr_pull, sqr_push)
