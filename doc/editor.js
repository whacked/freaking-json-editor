(function () {
    var keyClassø1 = 'key';
    var valClassø1 = 'value';
    var lsClassø1 = 'ls';
    var kvClassø1 = 'kv';
    void 0;
    var genEditor = function genEditor(parent, obj) {
        return function () {
            var isIsArrayø1 = Array.isArray(obj);
            var containerø1 = isIsArrayø1 ? $('<ol>', {
                'start': 0,
                'class': lsClassø1
            }) : $('<div>', { 'class': kvClassø1 });
            return $.each(obj, function (k, v) {
                return function () {
                    var rowø1 = isIsArrayø1 ? $('<li>') : $('<div>', { 'class': 'item' }).append($('<input>', {
                        'type': 'text',
                        'class': keyClassø1
                    }).val(k));
                    containerø1.append(rowø1);
                    typeof(v) === 'object' ? genEditor(rowø1, v) : rowø1.append($('<input>', {
                        'type': 'text',
                        'class': valClassø1 + ' ' + typeof(v)
                    }).val(v));
                    return parent.append(containerø1);
                }.call(this);
            });
        }.call(this);
    };
    return function () {
        var frmø1 = $('<form>');
        var taø1 = $('<textarea>');
        var mainRowø1 = $('<div>', { 'class': 'viewer-row' });
        var mainViewø1 = $('<div>', { 'class': 'viewer-main' }).append(mainRowø1);
        taø1.val(JSON.stringify([
            'one',
            {
                '1': 'some number',
                'a': 1,
                'b': {
                    'foo': 'bar',
                    'baz': 'quux'
                },
                'x': [
                    1,
                    2,
                    3
                ],
                'y': {
                    'nested': 'map',
                    'within': [
                        'another array',
                        'cool',
                        { 'yet': 'more' },
                        1,
                        {
                            'snowman': [
                                'five',
                                5,
                                'f5'
                            ]
                        },
                        { 'ice': { 'cream': 'cone' } }
                    ]
                }
            },
            'tow'
        ], void 0, 4));
        mainRowø1.append($('<div>', { 'class': 'viewer-cell' }).append(taø1)).append($('<div>', { 'class': 'viewer-cell' }).append(frmø1));
        $('#content').empty().append(mainViewø1);
        var formToJson = function formToJson($domel, rtn) {
            return $domel.prop('tagName') === 'INPUT' ? $domel.hasClass('number') ? parseInt($domel.val()) : $domel.val() : function () {
                var rtnø2 = rtn || ($domel.hasClass(lsClassø1) || 0 < $domel.children(lsClassø1)['length'] ? [] : $domel.hasClass(kvClassø1) ? {} : []);
                var isPushingToArrayø1 = Array.isArray(rtnø2);
                (function loop() {
                    var recur = loop;
                    var elArrø1 = $domel.children();
                    var keyø1 = void 0;
                    do {
                        recur = 0 < elArrø1['length'] ? function () {
                            var $elø1 = $(elArrø1[0]);
                            var elValø1 = $elø1.val();
                            var elTagø1 = $elø1.prop('tagName');
                            return elTagø1 === 'INPUT' ? isPushingToArrayø1 ? rtnø2.push(formToJson($elø1)) : rtnø2[elValø1] = formToJson($(elArrø1[1]), void 0) : function () {
                                isPushingToArrayø1 && $elø1.hasClass(kvClassø1) ? rtnø2.push(formToJson($elø1, void 0)) : formToJson($elø1, rtnø2);
                                return loop[0] = elArrø1.slice(1), loop[1] = keyø1, loop;
                            }.call(this);
                        }.call(this) : void 0;
                    } while (elArrø1 = loop[0], keyø1 = loop[1], recur === loop);
                    return recur;
                }.call(this));
                return rtnø2;
            }.call(this);
        };
        var respitJson = function respitJson() {
            return function () {
                var finalø1 = formToJson(frmø1);
                return taø1.val(JSON.stringify(finalø1, void 0, 4));
            }.call(this);
        };
        taø1.change(function (_) {
            return function () {
                var textø1 = taø1.val();
                var jsonø1 = JSON.parse(textø1);
                genEditor(frmø1.empty(), jsonø1);
                return frmø1.find('input').change(respitJson);
            }.call(this);
        });
        taø1.change();
        return respitJson();
    }.call(this);
}.call(this));
//# sourceMappingURL=data:application/json;base64,eyJ2ZXJzaW9uIjozLCJzb3VyY2VzIjpbImFub255bW91cy53aXNwIl0sIm5hbWVzIjpbImtleUNsYXNzw7gxIiwidmFsQ2xhc3PDuDEiLCJsc0NsYXNzw7gxIiwia3ZDbGFzc8O4MSIsImdlbkVkaXRvciIsInBhcmVudCIsIm9iaiIsImlzSXNBcnJhecO4MSIsIkFycmF5IiwiaXNBcnJheSIsImNvbnRhaW5lcsO4MSIsIiQiLCJlYWNoIiwiayIsInYiLCJyb3fDuDEiLCJhcHBlbmQiLCJ2YWwiLCJ0eXBlb2YiLCJmcm3DuDEiLCJ0YcO4MSIsIm1haW5Sb3fDuDEiLCJtYWluVmlld8O4MSIsIkpTT04iLCJzdHJpbmdpZnkiLCJlbXB0eSIsImZvcm1Ub0pzb24iLCIkZG9tZWwiLCJydG4iLCJwcm9wIiwiaGFzQ2xhc3MiLCJwYXJzZUludCIsInJ0bsO4MiIsImNoaWxkcmVuIiwiaXNQdXNoaW5nVG9BcnJhecO4MSIsImVsQXJyw7gxIiwia2V5w7gxIiwiJGVsw7gxIiwiZWxWYWzDuDEiLCJlbFRhZ8O4MSIsInB1c2giLCJzbGljZSIsInJlc3BpdEpzb24iLCJmaW5hbMO4MSIsIkpTT04uc3RyaW5naWZ5IiwiY2hhbmdlIiwiXyIsInRleHTDuDEiLCJqc29uw7gxIiwicGFyc2UiLCJmaW5kIl0sIm1hcHBpbmdzIjoiQUFHQSxDLFlBQ007QUFBQSxRQUFBQSxVLEdBQVUsS0FBVjtBQUFBLElBQ0EsSUFBQUMsVSxHQUFVLE9BQVYsQ0FEQTtBQUFBLElBR0EsSUFBQUMsUyxHQUFTLElBQVQsQ0FIQTtBQUFBLElBSUEsSUFBQUMsUyxHQUFTLElBQVQsQ0FKQTtBQUFBLEksT0FBQTtBQUFBLElBZUosSUFBTUMsU0FBQSxHQUFOLFNBQU1BLFNBQU4sQ0FBa0JDLE1BQWxCLEVBQXlCQyxHQUF6QixFQUNFO0FBQUEsZSxZQUFNO0FBQUEsZ0JBQUFDLFcsR0FBV0MsS0FBQSxDQUFNQyxPQUFQLENBQWVILEdBQWYsQ0FBVjtBQUFBLFlBQ0EsSUFBQUksVyxHQUFjSCxXQUFKLEdBQ0dJLENBQUQsQ0FBRyxNQUFILEVBQVU7QUFBQSxnQixTQUFRLENBQVI7QUFBQSxnQixTQUNRVCxTQURSO0FBQUEsYUFBVixDQURGLEdBR0dTLENBQUQsQ0FBRyxPQUFILEVBQVcsRSxTQUFRUixTQUFSLEVBQVgsQ0FIWixDQURBO0FBQUEsWUFLSixPQUFDUSxDQUFBLENBQUVDLElBQUgsQ0FDQ04sR0FERCxFQUVDLFVBQUtPLENBQUwsRUFBT0MsQ0FBUCxFQUNFO0FBQUEsdUIsWUFBTTtBQUFBLHdCQUFBQyxLLEdBQVFSLFdBQUosR0FDT0ksQ0FBRCxDQUFHLE1BQUgsQ0FETixHQUVPQSxDQUFELENBQUcsT0FBSCxFQUFXLEUsU0FBUSxNQUFSLEVBQVgsQ0FDQyxDQUFDSyxNQUROLENBQ2tCTCxDQUFELENBQUcsU0FBSCxFQUNHO0FBQUEsd0IsUUFBTyxNQUFQO0FBQUEsd0IsU0FDUVgsVUFEUjtBQUFBLHFCQURILENBR0MsQ0FBQ2lCLEdBSE4sQ0FHVUosQ0FIVixDQURiLENBRk47QUFBQSxvQkFPQUgsV0FDQyxDQUFDTSxNQUROLENBQ2FELEtBRGIsRUFQSTtBQUFBLG9CQVNhRyxNQUFELENBQVFKLENBQVIsQ0FBWixLQUF1QixRQUEzQixHQUNHVixTQUFELENBQVlXLEtBQVosRUFBZ0JELENBQWhCLENBREYsR0FFV0MsS0FBUixDQUFDQyxNQUFGLENBQWtCTCxDQUFELENBQUcsU0FBSCxFQUNHO0FBQUEsd0IsUUFBTyxNQUFQO0FBQUEsd0IsU0FDV1YsVSxHQUFVLEdBQWIsR0FBa0JpQixNQUFELENBQVFKLENBQVIsQ0FEekI7QUFBQSxxQkFESCxDQUdDLENBQUNHLEdBSE4sQ0FHVUgsQ0FIVixDQUFiLENBRkYsQ0FUSTtBQUFBLG9CQWVKLE9BQVNULE1BQVIsQ0FBQ1csTUFBRixDQUFnQk4sV0FBaEIsRUFmSTtBQUFBLGlCLEtBQU4sQyxJQUFBO0FBQUEsYUFISCxFQUxJO0FBQUEsUyxLQUFOLEMsSUFBQTtBQUFBLEtBREYsQ0FmSTtBQUFBLElBMkNKLE8sWUFDTTtBQUFBLFlBQUFTLEssR0FBS1IsQ0FBRCxDQUFHLFFBQUgsQ0FBSjtBQUFBLFFBQ0EsSUFBQVMsSSxHQUFJVCxDQUFELENBQUcsWUFBSCxDQUFILENBREE7QUFBQSxRQUdBLElBQUFVLFMsR0FBY1YsQ0FBRCxDQUFHLE9BQUgsRUFBVyxFLFNBQVEsWUFBUixFQUFYLENBQWIsQ0FIQTtBQUFBLFFBSUEsSUFBQVcsVSxHQUFlWCxDQUFELENBQUcsT0FBSCxFQUFXLEUsU0FBUSxhQUFSLEVBQVgsQ0FDQyxDQUFDSyxNQUROLENBQ2FLLFNBRGIsQ0FBVixDQUpBO0FBQUEsUUFTRUQsSUFBTCxDQUFDSCxHQUFGLENBQ09NLElBQUEsQ0FBS0MsU0FBTixDQUNDO0FBQUEsWUFBQyxLQUFEO0FBQUEsWUFDQztBQUFBLGdCLEtBSUcsYUFKSDtBQUFBLGdCLEtBQ0ksQ0FESjtBQUFBLGdCLEtBRUk7QUFBQSxvQixPQUFNLEtBQU47QUFBQSxvQixPQUNNLE1BRE47QUFBQSxpQkFGSjtBQUFBLGdCLEtBS0s7QUFBQSxvQkFBQyxDQUFEO0FBQUEsb0JBQUcsQ0FBSDtBQUFBLG9CQUFLLENBQUw7QUFBQSxpQkFMTDtBQUFBLGdCLEtBT0s7QUFBQSxvQixVQUNTLEtBRFQ7QUFBQSxvQixVQUdVO0FBQUEsd0JBQUMsZUFBRDtBQUFBLHdCQUNDLE1BREQ7QUFBQSx3QkFFQyxFLE9BQU0sTUFBTixFQUZEO0FBQUEsd0JBR0MsQ0FIRDtBQUFBLHdCQUlDO0FBQUEsNEIsV0FBVTtBQUFBLGdDQUFDLE1BQUQ7QUFBQSxnQ0FBUSxDQUFSO0FBQUEsZ0NBQVUsSUFBVjtBQUFBLDZCQUFWO0FBQUEseUJBSkQ7QUFBQSx3QkFLQyxFLE9BQU0sRSxTQUFRLE1BQVIsRUFBTixFQUxEO0FBQUEscUJBSFY7QUFBQSxpQkFQTDtBQUFBLGFBREQ7QUFBQSxZQW1CQyxLQW5CRDtBQUFBLFNBREQsRSxNQUFBLEVBcUJLLENBckJMLENBRE4sRUFUSTtBQUFBLFFBaUNBSCxTQUNDLENBQUNMLE0sQ0FBWUwsQ0FBRCxDQUFHLE9BQUgsRUFBVyxFLFNBQVEsYUFBUixFQUFYLENBQ0MsQ0FBQ0ssTUFETixDQUNhSSxJQURiLEMsQ0FFUixDQUFDSixNQUhOLENBR2tCTCxDQUFELENBQUcsT0FBSCxFQUFXLEUsU0FBUSxhQUFSLEVBQVgsQ0FDQyxDQUFDSyxNQUROLENBQ2FHLEtBRGIsQ0FIYixFQWpDSTtBQUFBLFFBdUNDUixDQUFELENBQUcsVUFBSCxDQUNDLENBQUNjLEssRUFDRCxDQUFDVCxNQUZOLENBRWFNLFVBRmIsRUF2Q0k7QUFBQSxRQTJDSixJQUFNSSxVQUFBLEdBQU4sU0FBTUEsVUFBTixDQUFvQkMsTUFBcEIsRUFBMkJDLEdBQTNCLEVBQ0U7QUFBQSxtQkFBdUJELE1BQU4sQ0FBQ0UsSUFBRixDQUFjLFNBQWQsQ0FBWixLQUFxQyxPQUF6QyxHQUNpQkYsTUFBVixDQUFDRyxRQUFGLENBQWtCLFFBQWxCLENBQUosR0FDR0MsUUFBRCxDQUFnQkosTUFBTCxDQUFDVixHQUFGLEVBQVYsQ0FERixHQUVRVSxNQUFMLENBQUNWLEdBQUYsRUFISixHLFlBS1E7QUFBQSxvQkFBQWUsSyxHQUFRSixHQUFKLElBQ0ksQ0FBbUJELE1BQVYsQ0FBQ0csUUFBRixDQUFrQjVCLFNBQWxCLENBQUosSUFHTyxDQUFILEdBQXNCeUIsTUFBVixDQUFDTSxRQUFGLENBQWtCL0IsU0FBbEIsQ0FBTixDQUFrQyxRQUFsQyxDQUhiLEdBSUUsRUFKRixHQUtpQnlCLE1BQVYsQ0FBQ0csUUFBRixDQUFrQjNCLFNBQWxCLENBQUosR0FDRSxFQURGLEdBRUUsRUFQSixDQURSO0FBQUEsZ0JBVUEsSUFBQStCLGtCLEdBQW1CMUIsS0FBQSxDQUFNQyxPQUFQLENBQWV1QixLQUFmLENBQWxCLENBVkE7QUFBQSxnQkFjSixDOztvQkFBTyxJQUFBRyxPLEdBQWtCUixNQUFWLENBQUNNLFFBQUYsRUFBUCxDO3dCQUNBRyxLOztnQ0FDRSxDQUFILEdBQVdELE9BQU4sQ0FBYSxRQUFiLENBQVQsRyxZQUNRO0FBQUEsZ0NBQUFFLEssR0FBSzFCLENBQUQsQ0FBU3dCLE9BQU4sQ0FBYSxDQUFiLENBQUgsQ0FBSjtBQUFBLDRCQUNBLElBQUFHLE8sR0FBYUQsS0FBTCxDQUFDcEIsR0FBRixFQUFQLENBREE7QUFBQSw0QkFFQSxJQUFBc0IsTyxHQUFjRixLQUFOLENBQUNSLElBQUYsQ0FBVyxTQUFYLENBQVAsQ0FGQTtBQUFBLDRCQUlKLE9BQWdCVSxPQUFaLEtBQW1CLE9BQXZCLEdBR01MLGtCQUFKLEdBRVNGLEtBQU4sQ0FBQ1EsSUFBRixDQUFZZCxVQUFELENBQWNXLEtBQWQsQ0FBWCxDQUZGLEdBT2NMLEtBQU4sQ0FBVU0sT0FBVixDQUFOLEdBQ09aLFVBQUQsQ0FBZWYsQ0FBRCxDQUFTd0IsT0FBTixDQUFhLENBQWIsQ0FBSCxDQUFkLEUsTUFBQSxDQVhWLEcsWUFnQkk7QUFBQSxnQ0FBU0Qsa0JBQUwsSUFDZ0JHLEtBQVYsQ0FBQ1AsUUFBRixDQUFlM0IsU0FBZixDQURULEdBSVM2QixLQUFOLENBQUNRLElBQUYsQ0FBWWQsVUFBRCxDQUFjVyxLQUFkLEUsTUFBQSxDQUFYLENBSkYsR0FPR1gsVUFBRCxDQUFjVyxLQUFkLEVBQWtCTCxLQUFsQixDQVBGO0FBQUEsZ0NBUUEsTyxVQUFlRyxPQUFQLENBQUNNLEtBQUYsQ0FBZSxDQUFmLENBQVAsRSxVQUF5QkwsS0FBekIsRSxJQUFBLENBUkE7QUFBQSw2QixLQURGLEMsSUFBQSxDQWZGLENBSkk7QUFBQSx5QixLQUFOLEMsSUFBQSxDQURGLEc7NkJBRktELE8sWUFDQUMsSzs7c0JBRFAsQyxJQUFBLEdBZEk7QUFBQSxnQkErQ0osT0FBQUosS0FBQSxDQS9DSTtBQUFBLGEsS0FETixDLElBQUEsQ0FKRjtBQUFBLFNBREYsQ0EzQ0k7QUFBQSxRQW1HSixJQUFNVSxVQUFBLEdBQU4sU0FBTUEsVUFBTixHQUNFO0FBQUEsbUIsWUFBTTtBQUFBLG9CQUFBQyxPLEdBQU9qQixVQUFELENBQWNQLEtBQWQsQ0FBTjtBQUFBLGdCQUNKLE9BQU1DLElBQUwsQ0FBQ0gsR0FBRixDQUFVMkIsY0FBRCxDQUFnQkQsT0FBaEIsRSxNQUFBLEVBQTBCLENBQTFCLENBQVQsRUFESTtBQUFBLGEsS0FBTixDLElBQUE7QUFBQSxTQURGLENBbkdJO0FBQUEsUUF1R0t2QixJQUFSLENBQUN5QixNQUFGLENBQ1MsVUFBS0MsQ0FBTCxFQUNFO0FBQUEsbUIsWUFBTTtBQUFBLG9CQUFBQyxNLEdBQVczQixJQUFMLENBQUNILEdBQUYsRUFBTDtBQUFBLGdCQUNBLElBQUErQixNLEdBQU16QixJQUFBLENBQUswQixLQUFOLENBQVlGLE1BQVosQ0FBTCxDQURBO0FBQUEsZ0JBRUgzQyxTQUFELENBQW9CZSxLQUFQLENBQUNNLEtBQUYsRUFBWixFQUF5QnVCLE1BQXpCLEVBRkk7QUFBQSxnQkFHSixPQUFJN0IsS0FDQyxDQUFDK0IsSSxDQUFLLE8sQ0FDTixDQUFDTCxNQUZOLENBRWFILFVBRmIsRUFISTtBQUFBLGEsS0FBTixDLElBQUE7QUFBQSxTQUZYLEVBdkdJO0FBQUEsUUErR0t0QixJQUFSLENBQUN5QixNQUFGLEdBL0dJO0FBQUEsUUFpSEosT0FBQ0gsVUFBRCxHQWpISTtBQUFBLEssS0FETixDLElBQUEsRUEzQ0k7QUFBQSxDLEtBRE4sQyxJQUFBIiwic291cmNlc0NvbnRlbnQiOlsiOzsgKG5zIGVkaXRvci5tYWluXG47OyAgICg6cmVxdWlyZSBbd2lzcC5jb3JlXSkpXG5cbihsZXQgW1xuICAgICAga2V5LWNsYXNzIFwia2V5XCJcbiAgICAgIHZhbC1jbGFzcyBcInZhbHVlXCJcbiAgICAgIFxuICAgICAgbHMtY2xhc3MgXCJsc1wiXG4gICAgICBrdi1jbGFzcyBcImt2XCJcbiAgICAgIF1cbiAgKGRlZm1hY3JvIC0+XG4gICAgWyYgb3BlcmF0aW9uc11cbiAgICAocmVkdWNlXG4gICAgIChmbiBbZm9ybSBvcGVyYXRpb25dXG4gICAgICAgKGNvbnMgKGZpcnN0IG9wZXJhdGlvbilcbiAgICAgICAgICAgICAoY29ucyBmb3JtIChyZXN0IG9wZXJhdGlvbikpKSlcbiAgICAgKGZpcnN0IG9wZXJhdGlvbnMpXG4gICAgIChyZXN0IG9wZXJhdGlvbnMpKSlcbiAgXG4gIChkZWZuIGdlbi1lZGl0b3IgW3BhcmVudCBvYmpdXG4gICAgKGxldCBbaXMtYXJyYXk/IChBcnJheS5pc0FycmF5IG9iailcbiAgICAgICAgICBjb250YWluZXIgKGlmIGlzLWFycmF5P1xuICAgICAgICAgICAgICAgICAgICAgICgkIFwiPG9sPlwiIHs6c3RhcnQgMFxuICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgOmNsYXNzIGxzLWNsYXNzfSlcbiAgICAgICAgICAgICAgICAgICAgICAoJCBcIjxkaXY+XCIgezpjbGFzcyBrdi1jbGFzc30pKV1cbiAgICAgICgkLmVhY2hcbiAgICAgICBvYmpcbiAgICAgICAoZm4gW2sgdl1cbiAgICAgICAgIChsZXQgW3JvdyAoaWYgaXMtYXJyYXk/XG4gICAgICAgICAgICAgICAgICAgICAoLT4gKCQgXCI8bGk+XCIpKVxuICAgICAgICAgICAgICAgICAgICAgKC0+ICgkIFwiPGRpdj5cIiB7OmNsYXNzIFwiaXRlbVwifSlcbiAgICAgICAgICAgICAgICAgICAgICAgICAoLmFwcGVuZCAoLT4gKCQgXCI8aW5wdXQ+XCJcbiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgezp0eXBlIFwidGV4dFwiXG4gICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICA6Y2xhc3Mga2V5LWNsYXNzfSlcbiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgKC52YWwgaykpKSkpXVxuICAgICAgICAgICAoLT4gY29udGFpbmVyXG4gICAgICAgICAgICAgICAoLmFwcGVuZCByb3cpKVxuICAgICAgICAgICAoaWYgKGlkZW50aWNhbD8gKHR5cGVvZiB2KSBcIm9iamVjdFwiKVxuICAgICAgICAgICAgIChnZW4tZWRpdG9yIHJvdyB2KVxuICAgICAgICAgICAgICguYXBwZW5kIHJvdyAoLT4gKCQgXCI8aW5wdXQ+XCJcbiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIHs6dHlwZSBcInRleHRcIlxuICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIDpjbGFzcyAoKyB2YWwtY2xhc3MgXCIgXCIgKHR5cGVvZiB2KSl9KVxuICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgKC52YWwgdikpKSlcbiAgICAgICAgICAgKC5hcHBlbmQgcGFyZW50IGNvbnRhaW5lcikpKVxuICAgICAgICkpXG4gICAgKVxuXG4gIChsZXQgW1xuICAgICAgICBmcm0gKCQgXCI8Zm9ybT5cIilcbiAgICAgICAgdGEgKCQgXCI8dGV4dGFyZWE+XCIpXG5cbiAgICAgICAgbWFpbi1yb3cgKC0+ICgkIFwiPGRpdj5cIiB7OmNsYXNzIFwidmlld2VyLXJvd1wifSkpXG4gICAgICAgIG1haW4tdmlldyAoLT4gKCQgXCI8ZGl2PlwiIHs6Y2xhc3MgXCJ2aWV3ZXItbWFpblwifSlcbiAgICAgICAgICAgICAgICAgICAgICAoLmFwcGVuZCBtYWluLXJvdykpXG4gICAgICAgIF1cbiAgICBcbiAgICA7OyBkZW1vIGRhdGFcbiAgICAoLnZhbCB0YVxuICAgICAgICAgIChKU09OLnN0cmluZ2lmeVxuICAgICAgICAgICBbXCJvbmVcIlxuICAgICAgICAgICAge1xuICAgICAgICAgICAgIDphIDFcbiAgICAgICAgICAgICA6YiB7OmZvbyBcImJhclwiXG4gICAgICAgICAgICAgICAgIDpiYXogXCJxdXV4XCJ9XG4gICAgICAgICAgICAgMSBcInNvbWUgbnVtYmVyXCJcbiAgICAgICAgICAgICBcInhcIiBbMSAyIDNdXG5cbiAgICAgICAgICAgICBcInlcIiB7XG4gICAgICAgICAgICAgICAgICA6bmVzdGVkIFwibWFwXCJcblxuICAgICAgICAgICAgICAgICAgXCJ3aXRoaW5cIiBbXCJhbm90aGVyIGFycmF5XCJcbiAgICAgICAgICAgICAgICAgICAgICAgICAgICBcImNvb2xcIlxuICAgICAgICAgICAgICAgICAgICAgICAgICAgIHs6eWV0IFwibW9yZVwifVxuICAgICAgICAgICAgICAgICAgICAgICAgICAgIDFcbiAgICAgICAgICAgICAgICAgICAgICAgICAgICB7OnNub3dtYW4gW1wiZml2ZVwiIDUgXCJmNVwiXX1cbiAgICAgICAgICAgICAgICAgICAgICAgICAgICB7OmljZSB7OmNyZWFtIFwiY29uZVwifX1dXG4gICAgICAgICAgICAgICAgICB9XG4gICAgICAgICAgICAgfVxuICAgICAgICAgICAgXCJ0b3dcIl1cbiAgICAgICAgICAgbmlsIDQpKVxuXG4gICAgKC0+IG1haW4tcm93XG4gICAgICAgICguYXBwZW5kICgtPiAoJCBcIjxkaXY+XCIgezpjbGFzcyBcInZpZXdlci1jZWxsXCJ9KVxuICAgICAgICAgICAgICAgICAgICAgKC5hcHBlbmQgdGEpKSlcbiAgICAgICAgKC5hcHBlbmQgKC0+ICgkIFwiPGRpdj5cIiB7OmNsYXNzIFwidmlld2VyLWNlbGxcIn0pXG4gICAgICAgICAgICAgICAgICAgICAoLmFwcGVuZCBmcm0pKSkpXG5cbiAgICAoLT4gKCQgXCIjY29udGVudFwiKVxuICAgICAgICAoLmVtcHR5KVxuICAgICAgICAoLmFwcGVuZCBtYWluLXZpZXcpKVxuXG4gICAgKGRlZm4gZm9ybS10by1qc29uIFskZG9tZWwgcnRuXVxuICAgICAgKGlmIChpZGVudGljYWw/ICgucHJvcCAkZG9tZWwgXCJ0YWdOYW1lXCIpIFwiSU5QVVRcIilcbiAgICAgICAgKGlmICguaGFzQ2xhc3MgJGRvbWVsIFwibnVtYmVyXCIpXG4gICAgICAgICAgKHBhcnNlSW50ICgudmFsICRkb21lbCkpXG4gICAgICAgICAgKC52YWwgJGRvbWVsKSlcbiAgICAgICAgKGxldCBbXG4gICAgICAgICAgICAgIHJ0biAob3IgcnRuXG4gICAgICAgICAgICAgICAgICAgICAgKGlmIChvciAoLmhhc0NsYXNzICRkb21lbCBscy1jbGFzcylcbiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIDs7IGNvdWxkIGNvbmNpZXZhYmx5IGJlIG5lc3RlZCBkZWVwZXIsIGJ1dFxuICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgOzsgd2UnbGwgY2hlY2sgMSBsZXZlbCBkZWVwXG4gICAgICAgICAgICAgICAgICAgICAgICAgICAgICAoPCAwIChhZ2V0ICguY2hpbGRyZW4gJGRvbWVsIGxzLWNsYXNzKSBcImxlbmd0aFwiKSkpXG4gICAgICAgICAgICAgICAgICAgICAgICBbXVxuICAgICAgICAgICAgICAgICAgICAgICAgKGlmICguaGFzQ2xhc3MgJGRvbWVsIGt2LWNsYXNzKVxuICAgICAgICAgICAgICAgICAgICAgICAgICB7fVxuICAgICAgICAgICAgICAgICAgICAgICAgICBbXSkpXG4gICAgICAgICAgICAgICAgICAgICAgKVxuICAgICAgICAgICAgICBwdXNoaW5nLXRvLWFycmF5PyAoQXJyYXkuaXNBcnJheSBydG4pXG4gICAgICAgICAgICAgIF1cbiAgICAgICAgICA7OyByZWFzb24gd2UgdXNlIGxvb3AgaGVyZSwgaXMgYmVjYXVzZSB3aGVuIHdlIGhpdCBhbiBpbnB1dCB0aGF0IGlzIGEga2V5LFxuICAgICAgICAgIDs7IHdlIHdhbnQgdG8gXCJzdG9yZVwiIHRoZSBrZXkgYW5kIG1vdmUgb24sIGJ1dCBkbyBpdCBmdW5jdGlvbmFsbHlcbiAgICAgICAgICAobG9vcCBbZWwtYXJyICguY2hpbGRyZW4gJGRvbWVsKVxuICAgICAgICAgICAgICAgICBrZXkgbmlsXVxuICAgICAgICAgICAgKGlmICg8IDAgKGFnZXQgZWwtYXJyIFwibGVuZ3RoXCIpKVxuICAgICAgICAgICAgICAobGV0IFskZWwgKCQgKGFnZXQgZWwtYXJyIDApKVxuICAgICAgICAgICAgICAgICAgICBlbC12YWwgKC52YWwgJGVsKVxuICAgICAgICAgICAgICAgICAgICBlbC10YWcgKC5wcm9wICRlbCBcInRhZ05hbWVcIildXG5cbiAgICAgICAgICAgICAgICAoaWYgKGlkZW50aWNhbD8gZWwtdGFnIFwiSU5QVVRcIilcbiAgICAgICAgICAgICAgICAgIFxuICAgICAgICAgICAgICAgICAgOzsgVFJVRVxuICAgICAgICAgICAgICAgICAgKGlmIHB1c2hpbmctdG8tYXJyYXk/XG4gICAgICAgICAgICAgICAgICAgIFxuICAgICAgICAgICAgICAgICAgICAoLnB1c2ggcnRuIChmb3JtLXRvLWpzb24gJGVsKSlcbiAgICAgICAgICAgICAgICAgICAgXG4gICAgICAgICAgICAgICAgICAgIDs7IHRoZSBmaXJzdCBpbnB1dCB0byBiZSBoaXQgZHVyaW5nIHRoZSBsb29wLFxuICAgICAgICAgICAgICAgICAgICA7OyBmb3IgYSBub24tYXJyYXksIG11c3QgYmUgYSBrZXkuIHdlIHRoZW5cbiAgICAgICAgICAgICAgICAgICAgOzsgZXhwZWN0IHRoZSByZW1haW5kZXIgdG8gYmUgZXhhY3RseSBsZW5ndGggMVxuICAgICAgICAgICAgICAgICAgICAoc2V0ISAoYWdldCBydG4gZWwtdmFsKVxuICAgICAgICAgICAgICAgICAgICAgICAgICAoZm9ybS10by1qc29uICgkIChhZ2V0IGVsLWFyciAxKSkgbmlsKSkpXG4gICAgICAgICAgICAgICAgICBcbiAgICAgICAgICAgICAgICAgIDs7IEVMU0VcbiAgICAgICAgICAgICAgICAgIDs7IG5vdCBpbnB1dCwgdHJhdmVyc2UgZGVlcGVyXG4gICAgICAgICAgICAgICAgICAobGV0IFtdXG4gICAgICAgICAgICAgICAgICAgIChpZiAoYW5kIHB1c2hpbmctdG8tYXJyYXk/XG4gICAgICAgICAgICAgICAgICAgICAgICAgICAgICguaGFzQ2xhc3MgJGVsIGt2LWNsYXNzKSlcbiAgICAgICAgICAgICAgICAgICAgICA7OyBUUlVFXG4gICAgICAgICAgICAgICAgICAgICAgOzsgbmV4dCBpcyBhIG1lbWJlciBvZiB0aGUgbGlzdCwgYW5kIHRoZSBtZW1iZXIgaXMgYW4gb2JqZWN0XG4gICAgICAgICAgICAgICAgICAgICAgKC5wdXNoIHJ0biAoZm9ybS10by1qc29uICRlbCBuaWwpKVxuICAgICAgICAgICAgICAgICAgICAgIDs7IG5leHQgaXMgYW4gb2JqZWN0IGF0IHRoZSBzYW1lIGxldmVsXG4gICAgICAgICAgICAgICAgICAgICAgOzsgRUxTRVxuICAgICAgICAgICAgICAgICAgICAgIChmb3JtLXRvLWpzb24gJGVsIHJ0bikpXG4gICAgICAgICAgICAgICAgICAgIChyZWN1ciAoLnNsaWNlIGVsLWFyciAxKSBrZXkpKSlcbiAgICAgICAgICAgICAgICApKSlcbiAgICAgICAgICBydG4pKVxuICAgICAgKVxuXG4gICAgKGRlZm4gcmVzcGl0LWpzb24gW11cbiAgICAgIChsZXQgW2ZpbmFsIChmb3JtLXRvLWpzb24gZnJtKV1cbiAgICAgICAgKC52YWwgdGEgKEpTT04vc3RyaW5naWZ5IGZpbmFsIG5pbCA0KSkpKVxuICAgIFxuICAgICguY2hhbmdlIHRhXG4gICAgICAgICAgICAgKGZuIFtfXVxuICAgICAgICAgICAgICAgKGxldCBbdGV4dCAoLnZhbCB0YSlcbiAgICAgICAgICAgICAgICAgICAgIGpzb24gKEpTT04ucGFyc2UgdGV4dCldXG4gICAgICAgICAgICAgICAgIChnZW4tZWRpdG9yICguZW1wdHkgZnJtKSBqc29uKVxuICAgICAgICAgICAgICAgICAoLT4gZnJtXG4gICAgICAgICAgICAgICAgICAgICAoLmZpbmQgXCJpbnB1dFwiKVxuICAgICAgICAgICAgICAgICAgICAgKC5jaGFuZ2UgcmVzcGl0LWpzb24pKSkpKVxuICAgICguY2hhbmdlIHRhKVxuXG4gICAgKHJlc3BpdC1qc29uKVxuICAgICkpXG4iXX0=
