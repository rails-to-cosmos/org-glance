const assert = require('node:assert/strict');
const fs = require('node:fs');
const vm = require('node:vm');

const context = {
    ObjC: {import() {}, bindFunction() {}, unwrap(value) {return value;}},
    $: {NSPasteboardTypeString: 'public.utf8-plain-text'}
};
vm.createContext(context);
vm.runInContext(fs.readFileSync(0, 'utf8'), context);

function cleanup(replaceAt) {
    let generation = 1, observedGeneration = 1;
    let token = 'lease', text = 'synthetic secret', checks = 0;
    const replace = () => {generation++; token = 'other'; text = 'replacement';};
    const board = {
        get changeCount() {
            checks++;
            const observed = generation;
            observedGeneration = generation;
            if (replaceAt === 'after-final-check' && checks === 2) replace();
            return observed;
        },
        stringForType() {
            const observed = token;
            if (replaceAt === 'before-final-check') replace();
            return observed;
        },
        setStringForType(value, type) {
            assert.equal(type, 'public.utf8-plain-text');
            if (replaceAt === 'during-write') replace();
            if (generation !== observedGeneration) return false;
            text = value;
            return true;
        },
        get clearContents() {generation++; token = null; text = null; return generation;}
    };
    if (replaceAt === 'before-cleanup') replace();
    context.clearOwned(board, 'lease');
    return text;
}

assert.equal(cleanup(null), '');
assert.equal(cleanup('before-cleanup'), 'replacement');
assert.equal(cleanup('before-final-check'), 'replacement');
assert.equal(cleanup('after-final-check'), 'replacement');
assert.equal(cleanup('during-write'), 'replacement');
