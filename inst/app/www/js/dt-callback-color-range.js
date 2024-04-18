//
// This JS file is intended to be used in the 'callback' option of a
// datatable in a R/Shiny App with the AutoFill extension activated.
// In this case it allows the user to select a range of the respective
// datatable which is colored and returned as a Shiny-input for further
// processing
//

// Define new DataTable AutoFill action (will be used as default)
$.fn.dataTable.AutoFill.actions.range = {
    available: function (dt, cells) {
        // Always available
        return !0
    },
    option: function (dt, cells) {
        // Default name of the action
        return 'Return range';
    },
    execute: function (dt, cells, node) {
        // do not modify the original values
    }
};

// remove dafault actions of the DataTable AutoFill plugin
delete $.fn.dataTable.AutoFill.actions.increment;
delete $.fn.dataTable.AutoFill.actions.fillHorizontal;
delete $.fn.dataTable.AutoFill.actions.fillVertical;
delete $.fn.dataTable.AutoFill.actions.fill;
delete $.fn.dataTable.AutoFill.actions.cancel;

// apply coloring of the selected cells and prepare shiny input
var tbl = $(table.table().node());
var id = tbl.closest('.datatables').attr('id');
table.on('autoFill', function(e, datatable, cells){
  // reset table cell background to default
  for (var i=0; i < datatable.rows().count(); ++i) {
    for (var j=0; j < datatable.columns().header().length; ++j) {
      $(table.cell(i, j).node()).css('background-color', '');
      $(table.cell(i, j).node()).css('color', '');
    }
  }
  // set up the output array to be returned to R/Shiny
  var out = [];
  for (var i=0; i<cells.length; ++i) {
    var cells_i = cells[i];
    for (var j=0; j < cells_i.length; ++j) {
      var c = cells_i[j];
      // provide array of selected cell indices
      out.push({row: c.index.row+1, col: c.index.column});
      // color the autofilled cells
      $(table.cell(c.index.row, c.index.column).node()).css('background-color', '#0d6efd');
      $(table.cell(c.index.row, c.index.column).node()).css('color', '#ffffff');
    }
  }
  Shiny.setInputValue(id + '_range_selected:DT.cellInfo', out);
});
