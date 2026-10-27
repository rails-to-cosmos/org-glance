;;; org-glance-clipboard-osx.el --- macOS -*- lexical-binding: t -*-

;;; Commentary:
;; Independent publisher and cleanup guardian for the macOS pasteboard.

;;; Code:

(require 'org-glance-clipboard)

(defconst org-glance-clipboard-osx--prelude
  "ObjC.import('AppKit'); ObjC.import('Foundation');
ObjC.bindFunction('clock_gettime_nsec_np', ['unsigned long long', ['int']]);
ObjC.bindFunction('getppid', ['int', []]);
ObjC.bindFunction('getpgrp', ['int', []]);
ObjC.bindFunction('setpgid', ['int', ['int', 'int']]);
function now() {
  var value = Number($.clock_gettime_nsec_np(4)) / 1e9;
  if (!(value > 0)) throw Error('Continuous clock unavailable');
  return value;
}
var leaseType = 'org.glance.clipboard.lease';
var deadlineType = 'org.glance.clipboard.deadline';
function owned(board, token) {
  return ObjC.unwrap(board.stringForType(leaseType)) === token;
}
function clearOwned(board, token) {
  var generation = board.changeCount;
  if (owned(board, token) && board.changeCount === generation)
    return board.setStringForType('', $.NSPasteboardTypeString);
  return false;
}
"
  "Shared JXA lease operations with a sleep-inclusive clock.
Expire text with an ownership-conditional write, preserving replacements.")

(defconst org-glance-clipboard-osx--publisher
  (concat org-glance-clipboard-osx--prelude
          "function run(args) {
  if ($.getpgrp() !== $.NSProcessInfo.processInfo.processIdentifier &&
      $.setpgid(0, 0) !== 0) throw Error('Cannot isolate publisher');
  var parent = $.getppid(), token = args[1];
  var data = $.NSFileHandle.fileHandleWithStandardInput.readDataToEndOfFile;
  var text = $.NSString.alloc.initWithDataEncoding(data, $.NSUTF8StringEncoding);
  var board = $.NSPasteboard.generalPasteboard;
  var deadline = now() + Number(args[0]);
  var item = $.NSPasteboardItem.alloc.init;
  item.setStringForType(text, $.NSPasteboardTypeString);
  item.setStringForType('', 'org.nspasteboard.ConcealedType');
  item.setStringForType('', 'org.nspasteboard.TransientType');
  item.setStringForType(token, leaseType);
  item.setStringForType(String(deadline), deadlineType);
  try {
    board.prepareForNewContentsWithOptions($.NSPasteboardContentsCurrentHostOnly);
    if (!board.writeObjects([item])) throw Error('Publication failed');
    while (now() < deadline && $.getppid() === parent && owned(board, token))
      $.NSThread.sleepForTimeInterval(.02);
  } finally { clearOwned(board, token); }
}")
  "Publish a text lease and expire it if the guardian exits or time expires.")

(defconst org-glance-clipboard-osx--script
  (concat org-glance-clipboard-osx--prelude
          "function run(args) {
  var token = ObjC.unwrap($.NSUUID.UUID.UUIDString);
  var board = $.NSPasteboard.generalPasteboard;
  var task = $.NSTask.alloc.init;
  task.launchPath = '/usr/bin/osascript';
  task.arguments = ['-l', 'JavaScript', '-e', args[1], args[0], token];
  task.standardInput = $.NSFileHandle.fileHandleWithStandardInput;
  task.standardOutput = $.NSFileHandle.fileHandleForWritingAtPath('/dev/null');
  task.standardError = task.standardOutput;
  var startup = now() + 4;
  try {
    task.launch;
    while (task.isRunning && now() < startup && !owned(board, token))
      $.NSThread.sleepForTimeInterval(.02);
    if (!task.isRunning || !owned(board, token)) throw Error('Publication failed');
    var deadline = Number(ObjC.unwrap(board.stringForType(deadlineType)));
    if (!(deadline > now())) throw Error('Expired publication');
    $.NSFileHandle.fileHandleWithStandardOutput.writeData(
      $('ready\\n').dataUsingEncoding($.NSUTF8StringEncoding));
    while (task.isRunning && now() < deadline && owned(board, token))
      $.NSThread.sleepForTimeInterval(.02);
  } finally {
    clearOwned(board, token);
    if (task.isRunning) task.terminate;
  }
}")
  "Acknowledge the published lease and expire its text if the publisher exits.")

(defun org-glance-clipboard-osx:copy (value)
  "Copy VALUE through a macOS publisher supervised by an independent guardian.
Cleanup replaces leased text through AppKit's ownership-conditional write."
  (org-glance-clipboard--send
   value (list "/usr/bin/osascript" "-l" "JavaScript" "-e"
               org-glance-clipboard-osx--script
               (number-to-string org-glance-clipboard-ttl)
               org-glance-clipboard-osx--publisher)))

(provide 'org-glance-clipboard-osx)
;;; org-glance-clipboard-osx.el ends here
