// Double click on a row of a table of wells: the position of the well (second
// column) is sent to the input named by the data-input attribute of the
// container, so that the well can be excluded or restored. The server decides
// whether the well can be (e.g. a well of a role not ticked cannot).
(function () {
    document.addEventListener('dblclick', function (event) {
        var container = event.target.closest('.dblclick-table');
        if (!container || !window.Shiny) return;
        var row = event.target.closest('tbody tr');
        if (!row || !container.contains(row) || row.cells.length < 2) return;
        // A double click selects a word: the row is the target, not the text
        if (window.getSelection) window.getSelection().removeAllRanges();
        Shiny.setInputValue(container.dataset.input, {
            key: row.cells[1].textContent.trim(),
            nonce: Date.now()
        }, {priority: 'event'});
    });
})();
