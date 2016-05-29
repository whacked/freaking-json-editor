var jsNull = exports.jsNull = eval('null');
(function () {
    var keyClassø1 = 'key';
    var valClassø1 = 'value';
    var lsClassø1 = 'ls';
    var kvClassø1 = 'kv';
    var nullClassø1 = 'null';
    void 0;
    var genEditor = function genEditor(parent, obj) {
        return function () {
            var isIsArrayø1 = Array.isArray(obj);
            var isIsNullø1 = obj === jsNull;
            var containerø1 = isIsArrayø1 ? $('<ol>', {
                'start': 0,
                'class': lsClassø1
            }) : isIsNullø1 ? $('<div>', { 'class': nullClassø1 }) : $('<div>', { 'class': kvClassø1 });
            parent.append(containerø1);
            return obj ? $.each(obj, function (k, v) {
                return function () {
                    var rowø1 = isIsArrayø1 ? $('<li>') : $('<div>', { 'class': 'item' }).append($('<input>', {
                        'type': 'text',
                        'class': keyClassø1
                    }).val(k));
                    containerø1.append(rowø1);
                    return typeof(v) === 'object' ? genEditor(rowø1, v) : rowø1.append((typeof(v) === 'string' ? $('<textarea>', { 'class': valClassø1 + ' ' + typeof(v) }) : $('<input>', {
                        'type': 'text',
                        'class': valClassø1 + ' ' + typeof(v)
                    })).val(v));
                }.call(this);
            }) : void 0;
        }.call(this);
    };
    var getChildDistance = function getChildDistance($parent, cls) {
        return $parent.hasClass(cls) ? 0 : function () {
            var distLsø1 = [];
            $parent.find('.' + cls).each(function (i, el) {
                return distLsø1.push($(el).parentsUntil($parent)['length']);
            });
            return Math.min.apply(void 0, distLsø1);
        }.call(this);
    };
    var formToJson = function formToJson($domel, rtn) {
        return $domel.prop('tagName') === 'INPUT' ? $domel.hasClass('number') ? parseInt($domel.val()) : $domel.val() : $domel.hasClass(nullClassø1) ? jsNull : function () {
            var rtnø2 = rtn || [
                [
                    getChildDistance($domel, lsClassø1),
                    []
                ],
                [
                    getChildDistance($domel, kvClassø1),
                    {}
                ]
            ].sort()[0][1];
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
                        return elTagø1 === 'INPUT' ? isPushingToArrayø1 ? rtnø2.push(formToJson($elø1)) : function () {
                            var nextø1 = elArrø1[1];
                            return rtnø2[elValø1] = formToJson($(nextø1), void 0);
                        }.call(this) : function () {
                            isPushingToArrayø1 && $elø1.hasClass(nullClassø1) ? rtnø2.push(formToJson($elø1, jsNull)) : isPushingToArrayø1 && $elø1.hasClass(kvClassø1) ? rtnø2.push(formToJson($elø1, void 0)) : formToJson($elø1, rtnø2);
                            return loop[0] = elArrø1.slice(1), loop[1] = keyø1, loop;
                        }.call(this);
                    }.call(this) : void 0;
                } while (elArrø1 = loop[0], keyø1 = loop[1], recur === loop);
                return recur;
            }.call(this));
            return rtnø2;
        }.call(this);
    };
    return function ($) {
        return $.fn['json_editor'] = function (opt) {
            return function () {
                var frmø1 = $('<form>');
                var isIsCustomTextø1 = opt && opt['text'];
                var taø1 = isIsCustomTextø1 ? $(opt['text']) : $('<textarea>', { 'class': 'editor' });
                isIsCustomTextø1 ? function () {
                    return $(this).append(frmø1);
                }.call(this) : function () {
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
                                3,
                                4
                            ],
                            'None': jsNull,
                            'emptylist': [],
                            'emptyobj': {},
                            'listwithnull': [
                                jsNull,
                                jsNull
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
                    return this.empty().append(mainViewø1);
                }.call(this);
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
        };
    }(jQuery);
}.call(this));
//# sourceMappingURL=data:application/json;base64,eyJ2ZXJzaW9uIjozLCJzb3VyY2VzIjpbImFub255bW91cy53aXNwIl0sIm5hbWVzIjpbImpzTnVsbCIsImV4cG9ydHMiLCJldmFsIiwia2V5Q2xhc3PDuDEiLCJ2YWxDbGFzc8O4MSIsImxzQ2xhc3PDuDEiLCJrdkNsYXNzw7gxIiwibnVsbENsYXNzw7gxIiwiZ2VuRWRpdG9yIiwicGFyZW50Iiwib2JqIiwiaXNJc0FycmF5w7gxIiwiQXJyYXkiLCJpc0FycmF5IiwiaXNJc051bGzDuDEiLCJjb250YWluZXLDuDEiLCIkIiwiYXBwZW5kIiwiZWFjaCIsImsiLCJ2Iiwicm93w7gxIiwidmFsIiwidHlwZW9mIiwiZ2V0Q2hpbGREaXN0YW5jZSIsIiRwYXJlbnQiLCJjbHMiLCJoYXNDbGFzcyIsImRpc3RMc8O4MSIsImZpbmQiLCJpIiwiZWwiLCJwdXNoIiwicGFyZW50c1VudGlsIiwiTWF0aCIsIm1pbi5hcHBseSIsImZvcm1Ub0pzb24iLCIkZG9tZWwiLCJydG4iLCJwcm9wIiwicGFyc2VJbnQiLCJydG7DuDIiLCJzb3J0IiwiaXNQdXNoaW5nVG9BcnJhecO4MSIsImVsQXJyw7gxIiwiY2hpbGRyZW4iLCJrZXnDuDEiLCIkZWzDuDEiLCJlbFZhbMO4MSIsImVsVGFnw7gxIiwibmV4dMO4MSIsInNsaWNlIiwiZm4iLCJvcHQiLCJmcm3DuDEiLCJpc0lzQ3VzdG9tVGV4dMO4MSIsInRhw7gxIiwidGhpcyIsIm1haW5Sb3fDuDEiLCJtYWluVmlld8O4MSIsIkpTT04iLCJzdHJpbmdpZnkiLCJlbXB0eSIsInJlc3BpdEpzb24iLCJmaW5hbMO4MSIsImNoYW5nZSIsIl8iLCJ0ZXh0w7gxIiwianNvbsO4MSIsInBhcnNlIiwialF1ZXJ5Il0sIm1hcHBpbmdzIjoiQUFBQSxJQUFLQSxNQUFBLEdBQUFDLE9BQUEsQ0FBQUQsTUFBQSxHQUFTRSxJQUFELENBQU0sTUFBTixDQUFiO0FBQ0EsQyxZQUNNO0FBQUEsUUFBQUMsVSxHQUFVLEtBQVY7QUFBQSxJQUNBLElBQUFDLFUsR0FBVSxPQUFWLENBREE7QUFBQSxJ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QUEwQixDQUExQixDQUFULEVBREk7QUFBQSxxQixLQUFOLEMsSUFBQTtBQUFBLGlCQURGLENBdERJO0FBQUEsZ0JBMERLUixJQUFSLENBQUNTLE1BQUYsQ0FDUyxVQUFLQyxDQUFMLEVBQ0U7QUFBQSwyQixZQUFNO0FBQUEsNEJBQUFDLE0sR0FBV1gsSUFBTCxDQUFDbEMsR0FBRixFQUFMO0FBQUEsd0JBQ0EsSUFBQThDLE0sR0FBTVIsSUFBQSxDQUFLUyxLQUFOLENBQVlGLE1BQVosQ0FBTCxDQURBO0FBQUEsd0JBRUgzRCxTQUFELENBQW9COEMsS0FBUCxDQUFDUSxLQUFGLEVBQVosRUFBeUJNLE1BQXpCLEVBRkk7QUFBQSx3QkFHSixPQUFJZCxLQUNDLENBQUN6QixJLENBQUssTyxDQUNOLENBQUNvQyxNQUZOLENBRWFGLFVBRmIsRUFISTtBQUFBLHFCLEtBQU4sQyxJQUFBO0FBQUEsaUJBRlgsRUExREk7QUFBQSxnQkFrRUtQLElBQVIsQ0FBQ1MsTUFBRixHQWxFSTtBQUFBLGdCQW9FSixPQUFDRixVQUFELEdBcEVJO0FBQUEsYSxLQUROLEMsSUFBQTtBQUFBLFNBRlI7QUFBQSxLQUZILENBOEVDTyxNQTlFRCxFQWxJSTtBQUFBLEMsS0FETixDLElBQUEiLCJzb3VyY2VzQ29udGVudCI6WyIoZGVmIGpzLW51bGwgKGV2YWwgXCJudWxsXCIpKVxuKGxldCBbXG4gICAgICBrZXktY2xhc3MgXCJrZXlcIlxuICAgICAgdmFsLWNsYXNzIFwidmFsdWVcIlxuICAgICAgXG4gICAgICBscy1jbGFzcyBcImxzXCJcbiAgICAgIGt2LWNsYXNzIFwia3ZcIlxuXG4gICAgICBudWxsLWNsYXNzIFwibnVsbFwiXG4gICAgICBdXG4gIChkZWZtYWNybyAtPlxuICAgIFsmIG9wZXJhdGlvbnNdXG4gICAgKHJlZHVjZVxuICAgICAoZm4gW2Zvcm0gb3BlcmF0aW9uXVxuICAgICAgIChjb25zIChmaXJzdCBvcGVyYXRpb24pXG4gICAgICAgICAgICAgKGNvbnMgZm9ybSAocmVzdCBvcGVyYXRpb24pKSkpXG4gICAgIChmaXJzdCBvcGVyYXRpb25zKVxuICAgICAocmVzdCBvcGVyYXRpb25zKSkpXG4gIFxuICAoZGVmbiBnZW4tZWRpdG9yIFtwYXJlbnQgb2JqXVxuICAgIChsZXQgW2lzLWFycmF5PyAoQXJyYXkuaXNBcnJheSBvYmopXG4gICAgICAgICAgaXMtbnVsbD8gKGlkZW50aWNhbD8gb2JqIGpzLW51bGwpXG4gICAgICAgICAgY29udGFpbmVyIChpZiBpcy1hcnJheT9cbiAgICAgICAgICAgICAgICAgICAgICAoJCBcIjxvbD5cIiB7OnN0YXJ0IDBcbiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIDpjbGFzcyBscy1jbGFzc30pXG4gICAgICAgICAgICAgICAgICAgICAgKGlmIGlzLW51bGw/XG4gICAgICAgICAgICAgICAgICAgICAgICAoJCBcIjxkaXY+XCIgezpjbGFzcyBudWxsLWNsYXNzfSlcbiAgICAgICAgICAgICAgICAgICAgICAgICgkIFwiPGRpdj5cIiB7OmNsYXNzIGt2LWNsYXNzfSkpKV1cbiAgICAgICguYXBwZW5kIHBhcmVudCBjb250YWluZXIpXG4gICAgICAoaWYgb2JqXG4gICAgICAgICgkLmVhY2hcbiAgICAgICAgIG9ialxuICAgICAgICAgKGZuIFtrIHZdXG4gICAgICAgICAgIChsZXQgW3JvdyAoaWYgaXMtYXJyYXk/XG4gICAgICAgICAgICAgICAgICAgICAgICgtPiAoJCBcIjxsaT5cIikpXG4gICAgICAgICAgICAgICAgICAgICAgICgtPiAoJCBcIjxkaXY+XCIgezpjbGFzcyBcIml0ZW1cIn0pXG4gICAgICAgICAgICAgICAgICAgICAgICAgICAoLmFwcGVuZCAoLT4gKCQgXCI8aW5wdXQ+XCJcbiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICB7OnR5cGUgXCJ0ZXh0XCJcbiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgOmNsYXNzIGtleS1jbGFzc30pXG4gICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgKC52YWwgaykpKSkpXVxuICAgICAgICAgICAgICgtPiBjb250YWluZXJcbiAgICAgICAgICAgICAgICAgKC5hcHBlbmQgcm93KSlcbiAgICAgICAgICAgICAoaWYgKGlkZW50aWNhbD8gKHR5cGVvZiB2KSBcIm9iamVjdFwiKVxuICAgICAgICAgICAgICAgKGdlbi1lZGl0b3Igcm93IHYpXG4gICAgICAgICAgICAgICAoLmFwcGVuZCByb3cgKC0+IChpZiAoaWRlbnRpY2FsPyAodHlwZW9mIHYpIFwic3RyaW5nXCIpXG4gICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgKCQgXCI8dGV4dGFyZWE+XCJcbiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICB7OmNsYXNzICgrIHZhbC1jbGFzcyBcIiBcIiAodHlwZW9mIHYpKX0pXG4gICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgKCQgXCI8aW5wdXQ+XCJcbiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICB7OnR5cGUgXCJ0ZXh0XCJcbiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgOmNsYXNzICgrIHZhbC1jbGFzcyBcIiBcIiAodHlwZW9mIHYpKX0pKVxuICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAoLnZhbCB2KSkpKSkpKSkpKVxuXG4gIDs7IGRvbid0IGtub3cgd2hldGhlciB0aGUgZnVsbCB0cmF2ZXJzYWwgaXMgbmVjZXNzYXJ5XG4gIDs7IGJ1dCAkLmNsb3Nlc3QoKSB0cmF2ZXJzZXMgdXAsIGFuZCB0aGVyZSBkb2Vzbid0IHNlZW1cbiAgOzsgdG8gYmUgb25lIHRoYXQgZG9lcyB0aGUgc2FtZSBpbiB0aGUgb3Bwb3NpdGUgZGlyZWN0aW9uXG4gIChkZWZuIGdldC1jaGlsZC1kaXN0YW5jZSBbJHBhcmVudCBjbHNdXG4gICAgKGlmICguaGFzQ2xhc3MgJHBhcmVudCBjbHMpXG4gICAgICAwXG4gICAgICAobGV0IFtkaXN0LWxzIFtdXVxuICAgICAgICAoLmVhY2hcbiAgICAgICAgICguZmluZCAkcGFyZW50ICgrIFwiLlwiIGNscykpXG4gICAgICAgICAoZm4gW2kgZWxdXG4gICAgICAgICAgICgucHVzaCBkaXN0LWxzIChhZ2V0ICgucGFyZW50c1VudGlsICgkIGVsKSAkcGFyZW50KSBcImxlbmd0aFwiKSkpKVxuICAgICAgICAoTWF0aC5taW4uYXBwbHkgbmlsIGRpc3QtbHMpKSkpXG5cbiAgKGRlZm4gZm9ybS10by1qc29uIFskZG9tZWwgcnRuXVxuICAgIDs7IGJldHRlciBvZmYgYSBjb25kIGJ1dCBmb3Igbm93LFxuICAgIDs7IDEuIGlmIElOUFVULCByZXR1cm4gdGhlIHR5cGVkIHZhbHVlIGl0IGNvbnRhaW5zXG4gICAgOzsgMi4gaWYgbnVsbCwgcmV0dXJuIG51bGwgLS0gbmljaGUgY2FzZVxuICAgIDs7IDMuIHByb2Nlc3MgcG90ZW50aWFsIGNoaWxkcmVuXG4gICAgKGlmIChpZGVudGljYWw/ICgucHJvcCAkZG9tZWwgOnRhZ05hbWUpIFwiSU5QVVRcIilcbiAgICAgIDs7IGdldCB2YWx1ZVxuICAgICAgKGlmICguaGFzQ2xhc3MgJGRvbWVsIFwibnVtYmVyXCIpXG4gICAgICAgIChwYXJzZUludCAoLnZhbCAkZG9tZWwpKVxuICAgICAgICAoLnZhbCAkZG9tZWwpKVxuICAgICAgXG4gICAgICAoaWYgKC5oYXNDbGFzcyAkZG9tZWwgbnVsbC1jbGFzcylcbiAgICAgICAgOzsgbmljaGUgY2FzZSBvZiBudWxsXG4gICAgICAgIGpzLW51bGxcblxuICAgICAgICA7OyBwb3RlbnRpYWwgY2hpbGRyZW5cbiAgICAgICAgKGxldCBbcnRuIChvciBydG5cbiAgICAgICAgICAgICAgICAgICAgICA7OyBtb3N0IGxpa2VseSBvbmUgaXMgdGhlIG9uZSB3aXRoIG1pbmltdW0gZGlzdGFuY2VcbiAgICAgICAgICAgICAgICAgICAgICAoLT4gW1soZ2V0LWNoaWxkLWRpc3RhbmNlICRkb21lbCBscy1jbGFzcykgW11dXG4gICAgICAgICAgICAgICAgICAgICAgICAgICBbKGdldC1jaGlsZC1kaXN0YW5jZSAkZG9tZWwga3YtY2xhc3MpIHt9XV1cbiAgICAgICAgICAgICAgICAgICAgICAgICAgKC5zb3J0KVxuICAgICAgICAgICAgICAgICAgICAgICAgICAoYWdldCAwKVxuICAgICAgICAgICAgICAgICAgICAgICAgICAoYWdldCAxKSkpXG4gICAgICAgICAgICAgIHB1c2hpbmctdG8tYXJyYXk/IChBcnJheS5pc0FycmF5IHJ0bildXG4gICAgICAgICAgOzsgcmVhc29uIHdlIHVzZSBsb29wIGhlcmUsIGlzIGJlY2F1c2Ugd2hlbiB3ZSBoaXQgYW4gaW5wdXQgdGhhdCBpcyBhIGtleSxcbiAgICAgICAgICA7OyB3ZSB3YW50IHRvIFwic3RvcmVcIiB0aGUga2V5IGFuZCBtb3ZlIG9uLCBidXQgZG8gaXQgZnVuY3Rpb25hbGx5XG4gICAgICAgICAgKGxvb3AgW2VsLWFyciAoLmNoaWxkcmVuICRkb21lbClcbiAgICAgICAgICAgICAgICAga2V5IG5pbF1cbiAgICAgICAgICAgIChpZiAoPCAwIChhZ2V0IGVsLWFyciBcImxlbmd0aFwiKSlcbiAgICAgICAgICAgICAgKGxldCBbJGVsICgkIChhZ2V0IGVsLWFyciAwKSlcbiAgICAgICAgICAgICAgICAgICAgZWwtdmFsICgudmFsICRlbClcbiAgICAgICAgICAgICAgICAgICAgZWwtdGFnICgucHJvcCAkZWwgOnRhZ05hbWUpXVxuXG4gICAgICAgICAgICAgICAgKGlmIChpZGVudGljYWw/IGVsLXRhZyBcIklOUFVUXCIpXG4gICAgICAgICAgICAgICAgICBcbiAgICAgICAgICAgICAgICAgIDs7IFRSVUVcbiAgICAgICAgICAgICAgICAgIChpZiBwdXNoaW5nLXRvLWFycmF5P1xuICAgICAgICAgICAgICAgICAgICBcbiAgICAgICAgICAgICAgICAgICAgKC5wdXNoIHJ0biAoZm9ybS10by1qc29uICRlbCkpXG4gICAgICAgICAgICAgICAgICAgIFxuICAgICAgICAgICAgICAgICAgICA7OyB0aGUgZmlyc3QgaW5wdXQgdG8gYmUgaGl0IGR1cmluZyB0aGUgbG9vcCxcbiAgICAgICAgICAgICAgICAgICAgOzsgZm9yIGEgbm9uLWFycmF5LCBtdXN0IGJlIGEga2V5LiB3ZSB0aGVuXG4gICAgICAgICAgICAgICAgICAgIDs7IGV4cGVjdCB0aGUgcmVtYWluZGVyIHRvIGJlIGV4YWN0bHkgbGVuZ3RoIDFcbiAgICAgICAgICAgICAgICAgICAgKGxldCBbbmV4dCAoYWdldCBlbC1hcnIgMSldXG4gICAgICAgICAgICAgICAgICAgICAgKHNldCEgKGFnZXQgcnRuIGVsLXZhbClcbiAgICAgICAgICAgICAgICAgICAgICAgICAgICAoZm9ybS10by1qc29uICgkIG5leHQpIG5pbCkpKSlcblxuICAgICAgICAgICAgICAgICAgOzsgRUxTRVxuICAgICAgICAgICAgICAgICAgOzsgbm90IGlucHV0LCB0cmF2ZXJzZSBkZWVwZXJcbiAgICAgICAgICAgICAgICAgIChsZXQgW11cbiAgICAgICAgICAgICAgICAgICAgOzsgYW5vdGhlciB1Z2x5IDMtd2F5IGNvbmQgcmVwbGFjZW1lbnQ7XG4gICAgICAgICAgICAgICAgICAgIDs7IHdlJ3JlIGhhbmRsaW5nIDIgY2FzZXMgd2l0aGluIHB1c2ggdG8gYXJyYXksXG4gICAgICAgICAgICAgICAgICAgIDs7IHRoYXQgb2YgbnVsbCwgdnMgdGhhdCBvZiBrdi4gVE9ETzogYmVhdXRpZnlcbiAgICAgICAgICAgICAgICAgICAgKGlmIChhbmQgcHVzaGluZy10by1hcnJheT9cbiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgKC5oYXNDbGFzcyAkZWwgbnVsbC1jbGFzcykpXG4gICAgICAgICAgICAgICAgICAgICAgKC5wdXNoIHJ0biAoZm9ybS10by1qc29uICRlbCBqcy1udWxsKSlcbiAgICAgICAgICAgICAgICAgICAgICAoaWYgKGFuZCBwdXNoaW5nLXRvLWFycmF5P1xuICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICguaGFzQ2xhc3MgJGVsIGt2LWNsYXNzKSlcbiAgICAgICAgICAgICAgICAgICAgICAgIDs7IFRSVUVcbiAgICAgICAgICAgICAgICAgICAgICAgIDs7IG5leHQgaXMgYSBtZW1iZXIgb2YgdGhlIGxpc3QsIGFuZCB0aGUgbWVtYmVyIGlzIGFuIG9iamVjdFxuICAgICAgICAgICAgICAgICAgICAgICAgKC5wdXNoIHJ0biAoZm9ybS10by1qc29uICRlbCBuaWwpKVxuICAgICAgICAgICAgICAgICAgICAgICAgOzsgbmV4dCBpcyBhbiBvYmplY3QgYXQgdGhlIHNhbWUgbGV2ZWxcbiAgICAgICAgICAgICAgICAgICAgICAgIDs7IEVMU0VcbiAgICAgICAgICAgICAgICAgICAgICAgIChmb3JtLXRvLWpzb24gJGVsIHJ0bikpKVxuICAgICAgICAgICAgICAgICAgICAocmVjdXIgKC5zbGljZSBlbC1hcnIgMSkga2V5KSkpKSkpXG4gICAgICAgICAgcnRuKSkpKVxuICBcbiAgKChmbiBbJF1cbiAgICAgXG4gICAgIChzZXQhIChhZ2V0ICQuZm4gXCJqc29uX2VkaXRvclwiKVxuICAgICAgICAgICAoZm4gW29wdF1cbiAgICAgICAgICAgICAobGV0IFtcbiAgICAgICAgICAgICAgICAgICBmcm0gKCQgXCI8Zm9ybT5cIilcbiAgICAgICAgICAgICAgICAgICBcbiAgICAgICAgICAgICAgICAgICBpcy1jdXN0b20tdGV4dD8gKGFuZCBvcHQgKGFnZXQgb3B0IFwidGV4dFwiKSlcblxuICAgICAgICAgICAgICAgICAgIHRhIChpZiBpcy1jdXN0b20tdGV4dD9cbiAgICAgICAgICAgICAgICAgICAgICAgICgkIChhZ2V0IG9wdCBcInRleHRcIikpXG4gICAgICAgICAgICAgICAgICAgICAgICAoJCBcIjx0ZXh0YXJlYT5cIiB7OmNsYXNzIFwiZWRpdG9yXCJ9KSlcbiAgICAgICAgICAgICAgICAgICBdXG4gICAgICAgICAgICAgICBcbiAgICAgICAgICAgICAgIChpZiBpcy1jdXN0b20tdGV4dD9cbiAgICAgICAgICAgICAgICAgKGxldCBbXVxuICAgICAgICAgICAgICAgICAgICguYXBwZW5kICgkIHRoaXMpXG4gICAgICAgICAgICAgICAgICAgICAgICAgICAgZnJtKSlcbiAgICAgICAgICAgICAgICAgKGxldCBbbWFpbi1yb3cgKC0+ICgkIFwiPGRpdj5cIiB7OmNsYXNzIFwidmlld2VyLXJvd1wifSkpXG4gICAgICAgICAgICAgICAgICAgICAgIG1haW4tdmlldyAoLT4gKCQgXCI8ZGl2PlwiIHs6Y2xhc3MgXCJ2aWV3ZXItbWFpblwifSlcbiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAoLmFwcGVuZCBtYWluLXJvdykpXG4gICAgICAgICAgICAgICAgICAgICAgIF1cbiAgICAgICAgICAgICAgICAgICA7OyBkZW1vIGRhdGFcbiAgICAgICAgICAgICAgICAgICAoLnZhbCB0YVxuICAgICAgICAgICAgICAgICAgICAgICAgIChKU09OLnN0cmluZ2lmeVxuICAgICAgICAgICAgICAgICAgICAgICAgICBbXCJvbmVcIlxuICAgICAgICAgICAgICAgICAgICAgICAgICAge1xuICAgICAgICAgICAgICAgICAgICAgICAgICAgIDphIDFcbiAgICAgICAgICAgICAgICAgICAgICAgICAgICA6YiB7OmZvbyBcImJhclwiXG4gICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIDpiYXogXCJxdXV4XCJ9XG4gICAgICAgICAgICAgICAgICAgICAgICAgICAgMSBcInNvbWUgbnVtYmVyXCJcbiAgICAgICAgICAgICAgICAgICAgICAgICAgICBcInhcIiBbMSAyIDMgNF1cbiAgICAgICAgICAgICAgICAgICAgICAgICAgICBcIk5vbmVcIiBqcy1udWxsXG4gICAgICAgICAgICAgICAgICAgICAgICAgICAgOmVtcHR5bGlzdCBbXVxuICAgICAgICAgICAgICAgICAgICAgICAgICAgIDplbXB0eW9iaiB7fVxuICAgICAgICAgICAgICAgICAgICAgICAgICAgIDpsaXN0d2l0aG51bGwgW2pzLW51bGwganMtbnVsbF1cbiAgICAgICAgICAgICAgICAgICAgICAgICAgICBcInlcIiB7XG4gICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICA6bmVzdGVkIFwibWFwXCJcblxuICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgXCJ3aXRoaW5cIiBbXCJhbm90aGVyIGFycmF5XCJcbiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICBcImNvb2xcIlxuICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIHs6eWV0IFwibW9yZVwifVxuICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIDFcbiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICB7OnNub3dtYW4gW1wiZml2ZVwiIDUgXCJmNVwiXX1cbiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICB7OmljZSB7OmNyZWFtIFwiY29uZVwifX1dXG4gICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICB9XG4gICAgICAgICAgICAgICAgICAgICAgICAgICAgfVxuICAgICAgICAgICAgICAgICAgICAgICAgICAgXCJ0b3dcIl1cbiAgICAgICAgICAgICAgICAgICAgICAgICAgbmlsIDQpKVxuXG4gICAgICAgICAgICAgICAgICAgKC0+IG1haW4tcm93XG4gICAgICAgICAgICAgICAgICAgICAgICguYXBwZW5kICgtPiAoJCBcIjxkaXY+XCIgezpjbGFzcyBcInZpZXdlci1jZWxsXCJ9KVxuICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgKC5hcHBlbmQgdGEpKSlcbiAgICAgICAgICAgICAgICAgICAgICAgKC5hcHBlbmQgKC0+ICgkIFwiPGRpdj5cIiB7OmNsYXNzIFwidmlld2VyLWNlbGxcIn0pXG4gICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAoLmFwcGVuZCBmcm0pKSkpXG4gICAgICAgICAgICAgICAgICAgKC0+IHRoaXNcbiAgICAgICAgICAgICAgICAgICAgICAgKC5lbXB0eSlcbiAgICAgICAgICAgICAgICAgICAgICAgKC5hcHBlbmQgbWFpbi12aWV3KSkpKVxuICAgICAgICAgICAgICAgXG4gICAgICAgICAgICAgICAoZGVmbiByZXNwaXQtanNvbiBbXVxuICAgICAgICAgICAgICAgICAobGV0IFtmaW5hbCAoZm9ybS10by1qc29uIGZybSldXG4gICAgICAgICAgICAgICAgICAgKC52YWwgdGEgKEpTT04uc3RyaW5naWZ5IGZpbmFsIG5pbCA0KSkpKVxuICAgICAgICAgICAgICAgXG4gICAgICAgICAgICAgICAoLmNoYW5nZSB0YVxuICAgICAgICAgICAgICAgICAgICAgICAgKGZuIFtfXVxuICAgICAgICAgICAgICAgICAgICAgICAgICAobGV0IFt0ZXh0ICgudmFsIHRhKVxuICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICBqc29uIChKU09OLnBhcnNlIHRleHQpXVxuICAgICAgICAgICAgICAgICAgICAgICAgICAgIChnZW4tZWRpdG9yICguZW1wdHkgZnJtKSBqc29uKVxuICAgICAgICAgICAgICAgICAgICAgICAgICAgICgtPiBmcm1cbiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgKC5maW5kIFwiaW5wdXRcIilcbiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgKC5jaGFuZ2UgcmVzcGl0LWpzb24pKSkpKVxuICAgICAgICAgICAgICAgKC5jaGFuZ2UgdGEpXG5cbiAgICAgICAgICAgICAgIChyZXNwaXQtanNvbilcbiAgICAgICAgICAgICAgIClcblxuICAgICAgICAgICAgICkpXG4gICAgIClcbiAgIGpRdWVyeSlcbiAgKVxuIl19
