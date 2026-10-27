function run(args) {
    var board = $.NSPasteboard.pasteboardWithName($(args[0]));
    function replace() {
        var task = $.NSTask.alloc.init;
        task.launchPath = '/usr/bin/osascript';
        task.arguments = ['-l', 'JavaScript', '-e',
            "ObjC.import('AppKit'); function run(a) {" +
            "var b = $.NSPasteboard.pasteboardWithName($(a[0]));" +
            "b.clearContents; b.setStringForType('replacement', $.NSPasteboardTypeString);}",
            args[0]];
        task.standardOutput = $.NSFileHandle.fileHandleForWritingAtPath('/dev/null');
        task.standardError = task.standardOutput;
        task.launch;
        task.waitUntilExit;
        if (task.terminationStatus !== 0) throw Error('Replacement failed');
    }
    try {
        ['none', 'before-cleanup', 'after-final-check', 'during-write'].forEach(function (race) {
            var item = $.NSPasteboardItem.alloc.init;
            item.setStringForType('synthetic secret', $.NSPasteboardTypeString);
            item.setStringForType('lease', leaseType);
            board.prepareForNewContentsWithOptions($.NSPasteboardContentsCurrentHostOnly);
            if (!board.writeObjects([item])) throw Error('Publication failed');
            var checks = 0;
            var proxy = {
                get changeCount() {
                    var observed = board.changeCount;
                    if (++checks === 2 && race === 'after-final-check') replace();
                    return observed;
                },
                stringForType: function (type) {return board.stringForType(type);},
                setStringForType: function (value, type) {
                    if (race === 'during-write') replace();
                    return board.setStringForType(value, type);
                },
                get clearContents() {throw Error('Cleanup must not clear the pasteboard');}
            };
            if (race === 'before-cleanup') replace();
            var written = clearOwned(proxy, 'lease');
            var expected = race === 'none' ? '' : 'replacement';
            if (ObjC.unwrap(board.stringForType($.NSPasteboardTypeString)) !== expected)
                throw Error('Unexpected clipboard text: ' + race);
            if (written !== (race === 'none')) throw Error('Unexpected write result: ' + race);
        });
    } finally {board.releaseGlobally;}
    return 'ok';
}
