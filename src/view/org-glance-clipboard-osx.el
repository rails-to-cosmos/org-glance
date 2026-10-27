;;; org-glance-clipboard-osx.el --- macOS -*- lexical-binding: t -*-

;;; Commentary:
;; Confidential clipboard support for macOS.

;;; Code:

(require 'org-glance-clipboard)

(defconst org-glance-clipboard-osx--script
  "ObjC.import('AppKit'); ObjC.import('Foundation');
function run(args) {
  var data = $.NSFileHandle.fileHandleWithStandardInput.readDataToEndOfFile;
  var text = $.NSString.alloc.initWithDataEncoding(data, $.NSUTF8StringEncoding);
  var board = $.NSPasteboard.generalPasteboard;
  board.prepareForNewContentsWithOptions($.NSPasteboardContentsCurrentHostOnly);
  var item = $.NSPasteboardItem.alloc.init;
  item.setStringForType(text, $.NSPasteboardTypeString);
  item.setStringForType('', 'org.nspasteboard.ConcealedType');
  item.setStringForType('', 'org.nspasteboard.TransientType');
  if (!board.writeObjects([item])) throw Error('Clipboard write failed');
  var generation = board.changeCount;
  try {
    $.NSFileHandle.fileHandleWithStandardOutput.writeData(
      $('ready\\n').dataUsingEncoding($.NSUTF8StringEncoding));
    $.NSThread.sleepForTimeInterval(Number(args[0]));
  } finally {
    if (board.changeCount === generation) board.clearContents;
  }
}"
  "JXA helper receiving plaintext through stdin and expiring its own copy.")

(defun org-glance-clipboard-osx:copy (value)
  "Copy VALUE to an expiring confidential macOS pasteboard item."
  (org-glance-clipboard--send
   value (list "/usr/bin/osascript" "-l" "JavaScript" "-e"
               org-glance-clipboard-osx--script
               (number-to-string org-glance-clipboard-ttl))
   :ready "ready"))

(provide 'org-glance-clipboard-osx)
;;; org-glance-clipboard-osx.el ends here
