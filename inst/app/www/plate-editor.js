// Selection of wells on an editable plate map (template step).
//
// Markup: a container .plate-editor[data-input] holds buttons [data-action]
// and a rendered table .plate-map.editable with td[data-well],
// th.plate-row[data-row], th.plate-col[data-col] and th.plate-corner.
// Click or drag selects wells (Ctrl/Cmd/Shift adds to the selection), a click
// on a row or column header selects the whole row or column, the corner
// selects the plate, Escape clears the selection. A button sends
// {action, wells, level, nonce} to the shiny input named in data-input; the
// level of a quality control comes from select[data-qc-level].
(function () {
    'use strict';

    var drag = null;

    function editorOf(el) {
        return el.closest('.plate-editor');
    }

    function cells(table) {
        return Array.prototype.slice.call(table.querySelectorAll('td[data-well]'));
    }

    function position(td) {
        return { row: td.parentElement.rowIndex, col: td.cellIndex };
    }

    function selected(editor) {
        return Array.prototype.slice.call(editor.querySelectorAll('td.selected'))
            .map(function (td) { return td.dataset.well; });
    }

    function update(editor) {
        var n = selected(editor).length;
        editor.querySelectorAll('[data-action]').forEach(function (b) { b.disabled = n === 0; });
        var counter = editor.querySelector('.plate-selection-count');
        if (counter) counter.textContent = (counter.dataset.template || '{n}').replace('{n}', n);
    }

    function selectRect(table, a, b, base) {
        var r1 = Math.min(a.row, b.row), r2 = Math.max(a.row, b.row);
        var c1 = Math.min(a.col, b.col), c2 = Math.max(a.col, b.col);
        cells(table).forEach(function (td) {
            var p = position(td);
            var inside = p.row >= r1 && p.row <= r2 && p.col >= c1 && p.col <= c2;
            td.classList.toggle('selected', inside || base.indexOf(td) >= 0);
        });
    }

    function selectWhere(table, test, additive) {
        var targets = cells(table).filter(test);
        var all = targets.every(function (td) { return td.classList.contains('selected'); });
        if (!additive) cells(table).forEach(function (td) { td.classList.remove('selected'); });
        targets.forEach(function (td) { td.classList.toggle('selected', !(additive && all)); });
    }

    document.addEventListener('mousedown', function (e) {
        var td = e.target.closest('.plate-map.editable td[data-well]');
        if (!td) return;
        e.preventDefault();
        var table = td.closest('table');
        var additive = e.ctrlKey || e.metaKey || e.shiftKey;
        var base = additive ? cells(table).filter(function (c) { return c.classList.contains('selected'); }) : [];
        if (additive && td.classList.contains('selected')) {
            td.classList.remove('selected');
            update(editorOf(table));
            return;
        }
        drag = { table: table, start: position(td), base: base };
        selectRect(table, drag.start, drag.start, base);
        update(editorOf(table));
    });

    document.addEventListener('mouseover', function (e) {
        if (!drag) return;
        var td = e.target.closest('td[data-well]');
        if (!td || td.closest('table') !== drag.table) return;
        selectRect(drag.table, drag.start, position(td), drag.base);
        update(editorOf(drag.table));
    });

    document.addEventListener('mouseup', function () { drag = null; });

    document.addEventListener('click', function (e) {
        var th = e.target.closest('.plate-map.editable th');
        if (th) {
            var table = th.closest('table');
            var additive = e.ctrlKey || e.metaKey || e.shiftKey;
            if (th.dataset.row) {
                selectWhere(table, function (td) { return td.dataset.well.replace(/[0-9]+$/, '') === th.dataset.row; }, additive);
            } else if (th.dataset.col) {
                selectWhere(table, function (td) { return td.dataset.well.replace(/^[A-Z]+/, '') === th.dataset.col; }, additive);
            } else if (th.classList.contains('plate-corner')) {
                selectWhere(table, function () { return true; }, false);
            }
            update(editorOf(table));
            return;
        }
        var button = e.target.closest('.plate-editor [data-action]');
        if (button && window.Shiny) {
            var editor = editorOf(button);
            var wells = selected(editor);
            if (wells.length === 0) return;
            var level = editor.querySelector('[data-qc-level]');
            Shiny.setInputValue(editor.dataset.input,
                { action: button.dataset.action, wells: wells, level: level ? level.value : null,
                  nonce: Date.now() },
                { priority: 'event' });
        }
    });

    document.addEventListener('keydown', function (e) {
        if (e.key !== 'Escape') return;
        document.querySelectorAll('.plate-editor').forEach(function (editor) {
            editor.querySelectorAll('td.selected').forEach(function (td) { td.classList.remove('selected'); });
            update(editor);
        });
    });

    // A re-rendered map has no selection: disable the buttons
    $(document).on('shiny:value', function (e) {
        setTimeout(function () {
            document.querySelectorAll('.plate-editor').forEach(update);
        }, 0);
    });
})();
