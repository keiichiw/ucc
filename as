#!/usr/bin/env python2.7

import sys
import os.path
import re
import struct
import argparse


srcs = {'_main': {0: 'br main'}}
filename = ''
library = 'libucc.s'
pos = 0

def error(msg):
    print >> sys.stderr, '{}:{}: error:'.format(filename, pos), msg
    print >> sys.stderr, '    ' + srcs[filename][pos]
    sys.exit(1)


# ----------------------------------------------------------------------
#       utility functions (mainly parsing)
# ----------------------------------------------------------------------

regs = {'$sp': 13, '$bp': 14, '$ip': 15}
for i in range(0, 16):
    regs['$' + str(i)] = i

def is_reg(operand):
    return operand in regs

def parse_imm(operand):
    try:
        imm = int(operand, 0)
        return True, imm
    except ValueError:
        return False, 0

def parse_float(operand):
    try:
        f = float(operand)
        return True, f
    except ValueError:
        return False, 0.0

def parse_addr(operand):
    m = re.match(r'(\$\w+)\s*([+-])\s*(\w+)$', operand)
    if m:
        base = m.group(1)
        offset = m.group(2) + m.group(3)
        if is_reg(base) and parse_imm(offset)[0]:
            return True, base, offset
    m = re.match(r'\$\w+$', operand)
    if m and is_reg(m.group()):
        return True, m.group(), '0'
    m = re.match(r'[+-]?\w+$', operand)
    if m and parse_imm(m.group())[0]:
        return True, '$0', m.group()
    return False, '$0', '0'

def try_parse_addr(operand):
    success, base, offset = parse_addr(operand)
    if success:
        return '{}, {}'.format(base, offset)
    return operand

def try_parse_memaccess(operand):
    m = re.match(r'\[(.*)\]$', operand)
    if m:
        return True, try_parse_addr(m.group(1))
    return False, ''

def check_operands_n(operands, n, m=-1):
    if len(operands) < n:
        error('too few operands')
    if len(operands) > max(n, m):
        error('too many operands')

def check_imm_range(imm, lo=-0x8000, hi=0x7fff):
    if not lo <= imm <= hi:
        error('immediate value exceeds valid range')

def reg_num(reg):
    if reg not in regs:
        error('invalid syntax')
    return regs[reg]

def mkop(op, a0, a1, a2, imm):
    a0 = reg_num(a0)
    a1 = reg_num(a1)
    a2 = reg_num(a2)
    success, imm = parse_imm(imm)
    if not success:
        error('invalid syntax')
    check_imm_range(imm)
    head = chr((op << 4) + a0) + chr((a1 << 4) + a2)
    tail = chr((imm >> 8) & 0xff) + chr(imm & 0xff)
    return head + tail

def parse(line):
    line = line.strip()
    m = re.match(r'\S+', line)
    mnemonic = m.group()
    t = line[m.end():].strip()
    operands = re.split(r',\s*', t)
    if operands == ['']:
        return mnemonic, []
    return mnemonic,  operands


# ----------------------------------------------------------------------
#       mnemonic definitions
# ----------------------------------------------------------------------

def on_add(operands):
    check_operands_n(operands, 4)
    return mkop(0, operands[0], operands[1], operands[2], operands[3])

def on_sub(operands):
    check_operands_n(operands, 3)
    return mkop(1, operands[0], operands[1], operands[2], '0')

def on_shift(operands):
    check_operands_n(operands, 4)
    return mkop(2, operands[0], operands[1], operands[2], operands[3])

def on_fneg(operands):
    check_operands_n(operands, 2)
    return mkop(3, operands[0], operands[1], '$0', '0')

def on_fadd(operands):
    check_operands_n(operands, 3)
    return mkop(4, operands[0], operands[1], operands[2], '0')

def on_fmul(operands):
    check_operands_n(operands, 3)
    return mkop(5, operands[0], operands[1], operands[2], '0')

# def on_finv(operands):
    # check_operands_n(operands, 2)
    # return mkop(6, operands[0], operands[1], '$0', '0')

# def on_fsqrt(operands):
    # check_operands_n(operands, 2)
    # return mkop(7, operands[0], operands[1], '$0', '0')

def on_load(operands):
    check_operands_n(operands, 3)
    return mkop(8, operands[0], '$0', operands[1], operands[2])

def on_store(operands):
    check_operands_n(operands, 3)
    return mkop(9, '$0', operands[0], operands[1], operands[2])

def on_read(operands):
    check_operands_n(operands, 1)
    return mkop(10, operands[0], '$0', '$0', '0')

def on_write(operands):
    check_operands_n(operands, 1)
    return mkop(11, '$0', operands[0], '$0', '0')

def on_beq(operands):
    check_operands_n(operands, 4)
    return mkop(12, operands[0], operands[1], operands[2], operands[3])

def on_ble(operands):
    check_operands_n(operands, 4)
    return mkop(13, operands[0], operands[1], operands[2], operands[3])

def on_dot_int(operands):
    check_operands_n(operands, 1)
    success, imm = parse_imm(operands[0])
    if not success:
        error('invalid syntax')
    check_imm_range(imm, -0x80000000, 0xffffffff)
    head = chr((imm >> 24) & 0xff) + chr((imm >> 16) & 0xff)
    tail = chr((imm >>  8) & 0xff) + chr( imm        & 0xff)
    return head + tail

def on_dot_float(operands):
    check_operands_n(operands, 1)
    success, f = parse_float(operands[0])
    if not success:
        error('invalid syntax')
    try:
        s = struct.pack('>f', f)
        imm = struct.unpack('>i', s)[0]
        return on_dot_int([str(imm)])
    except OverflowError:
        error('float too large')

table = {
    'add':      on_add,
    'sub':      on_sub,
    'shift':    on_shift,
    'fneg':     on_fneg,
    'fadd':     on_fadd,
    'fmul':     on_fmul,
    # 'finv':     on_finv,
    # 'fsqrt':    on_fsqrt,
    'load':     on_load,
    'store':    on_store,
    'read':     on_read,
    'write':    on_write,
    'beq':      on_beq,
    'ble':      on_ble,
    '.int':     on_dot_int,
    '.float':   on_dot_float
}


# ----------------------------------------------------------------------
#       macro definitions
# ----------------------------------------------------------------------

def expand_nop(operands):
    check_operands_n(operands, 0)
    return ['add $0, $0, $0, 0']

def expand_mov(operands):
    check_operands_n(operands, 2)
    success, ret = try_parse_memaccess(operands[1])
    if success:
        return ['load {}, {}'.format(operands[0], ret)]
    success, ret = try_parse_memaccess(operands[0])
    if success:
        return ['store {}, {}'.format(operands[1], ret)]
    ret = try_parse_addr(operands[1])
    return ['add {}, $0, {}'.format(operands[0], ret)]

def expand_add(operands):
    check_operands_n(operands, 3, 4)
    if len(operands) == 4:
        return ['add {}'.format(', '.join(operands))]
    if is_reg(operands[2]):
        return ['add {}, {}, {}, 0'.format(operands[0], operands[1], operands[2])]
    if parse_imm(operands[2])[0]:
        return ['add {}, {}, $0, {}'.format(operands[0], operands[1], operands[2])]
    error('invalid syntax')

def expand_sub(operands):
    check_operands_n(operands, 3)
    success, imm = parse_imm(operands[2])
    if success:
        return ['add {}, {}, $0, {}'.format(operands[0], operands[1], str(-imm))]
    return ['sub {}'.format(', '.join(operands))]

def expand_neg(operands):
    check_operands_n(operands, 2)
    return ['sub {}, $0, {}'.format(operands[0], operands[1])]

def expand_shift(operands):
    check_operands_n(operands, 3, 4)
    if len(operands) == 4:
        return ['shift {}'.format(', '.join(operands))]
    success, base, offset = parse_addr(operands[2])
    if not success:
        error('invalid syntax')
    return ['shift {}, {}, {}, {}'.format(operands[0], operands[1], base, offset)]

def expand_shl(operands):
    check_operands_n(operands, 3)
    return ['shift {}, {}, $0, {}'.format(operands[0], operands[1], operands[2])]

def expand_shr(operands):
    check_operands_n(operands, 3)
    success, imm = parse_imm(operands[2])
    if success:
        return ['shift {}, {}, $0, {}'.format(operands[0], operands[1], str(-imm))]
    error('invalid syntax')

def expand_fsub(operands):
    check_operands_n(operands, 3)
    return [
        'fneg {}, {}'.format(operands[2], operands[2]),
        'fadd {}, {}, {}'.format(operands[0], operands[1], operands[2]),
        'fneg {}, {}'.format(operands[2], operands[2])
    ]

def expand_br(operands):
    check_operands_n(operands, 1)
    ret = try_parse_addr(operands[0])
    return ['beq $0, $0, {}'.format(ret)]

def expand_beq(operands):
    check_operands_n(operands, 3, 4)
    if len(operands) == 4:
        return ['beq {}'.format(', '.join(operands))]
    ret = try_parse_addr(operands[2])
    return ['beq {}, {}, {}'.format(operands[0], operands[1], ret)]

def expand_ble(operands):
    check_operands_n(operands, 3, 4)
    if len(operands) == 4:
        return ['ble {}'.format(', '.join(operands))]
    ret = try_parse_addr(operands[2])
    return ['ble {}, {}, {}'.format(operands[0], operands[1], ret)]

def expand_bge(operands):
    check_operands_n(operands, 3)
    ret = try_parse_addr(operands[2])
    return ['ble {}, {}, {}'.format(operands[1], operands[0], ret)]

def expand_push(operands):
    check_operands_n(operands, 1)
    return [
        'add $sp, $sp, $0, -1',
        'store {}, $sp, 0'.format(operands[0])
    ]

def expand_pop(operands):
    check_operands_n(operands, 1)
    return [
        'load {}, $sp, 0'.format(operands[0]),
        'add $sp, $sp, $0, 1'
    ]

def expand_call(operands):
    check_operands_n(operands, 1)
    ret = try_parse_addr(operands[0])
    return [
        'store $bp, $sp, -1',
        'store $ip, $sp, -2',
        'add $sp, $sp, $0, -2',
        'add $bp, $sp, $0, 0',
        'beq $0, $0, {}'.format(ret),
        'add $sp, $bp, $0, 2',
        'load $bp, $sp, -1'
    ]

def expand_ret(operands):
    check_operands_n(operands, 0)
    return [
        'load $12, $bp, 0',
        'beq $0, $0, $12, 4'
    ]

def expand_halt(operands):
    check_operands_n(operands, 0)
    return ['beq $0, $0, $ip, 0']

def expand_dot_data(operands):
    check_operands_n(operands, 0)
    return []

def expand_dot_text(operands):
    check_operands_n(operands, 0)
    return []

def expand_dot_int(operands):
    check_operands_n(operands, 1, 2)
    if len(operands) == 1:
        return ['.int {}'.format(operands[0])]
    success, imm = parse_imm(operands[1])
    if not success:
        error('invalid syntax')
    check_imm_range(imm, 0, 1000)
    return ['.int {}'.format(operands[0])] * imm

def expand_dot_float(operands):
    check_operands_n(operands, 1, 2)
    if len(operands) == 1:
        return ['.float {}'.format(operands[0])]
    success, imm = parse_imm(operands[1])
    if not success:
        error('invalid syntax')
    check_imm_range(imm, 0, 1000)
    return ['.float {}'.format(operands[0])] * imm

macro_table = {
    'nop':      expand_nop,
    'mov':      expand_mov,
    'add':      expand_add,
    'sub':      expand_sub,
    'neg':      expand_neg,
    'shift':    expand_shift,
    'shl':      expand_shl,
    'shr':      expand_shr,
    'fsub':     expand_fsub,
    'br':       expand_br,
    'beq':      expand_beq,
    'ble':      expand_ble,
    'bge':      expand_bge,
    'push':     expand_push,
    'pop':      expand_pop,
    'call':     expand_call,
    'ret':      expand_ret,
    'halt':     expand_halt,
    '.data':    expand_dot_data,
    '.text':    expand_dot_text,
    '.int':     expand_dot_int,
    '.float':   expand_dot_float
}


# ----------------------------------------------------------------------
#       label resolution
# ----------------------------------------------------------------------

labels = {}
rev_labels = {}

def add_label(label, i):
    dic = labels.get(label, {})
    if filename in dic and dic[filename][0] >= 0:
        error('duplicate declaration of label \'{}\''.format(label))
    val = dic.get(filename, [-1, False, False])
    dic[filename] = [i, val[1], False]
    labels[label] = dic
    rev_labels[i] = rev_labels.get(i, []) + [label]

def add_global(label):
    dic = labels.get(label, {})
    val = dic.get(filename, [-1, False, False])
    dic[filename] = [val[0], True, False]
    labels[label] = dic

def check_global(label):
    if labels[label][filename][0] < 0:
        error('label \'{}\' is not declared'.format(label))

def subst(label, cur):
    if is_reg(label) or parse_imm(label)[0] or parse_float(label)[0]:
        return [label]
    if label not in labels:
        error('label \'{}\' is not declared'.format(label))
    if filename in labels[label]:
        labels[label][filename][2] = True
        return ['$ip', str(labels[label][filename][0] - cur)]
    else:
        decl = ''
        for key in labels[label]:
            if labels[label][key][1]:
                if decl:
                    error('label \'{}\' is declared in multiple files ({}, {})'.format(label, decl, key))
                decl = key
        if not decl:
            error('label \'{}\' is not declared'.format(label))
        labels[label][decl][2] = True
        return ['$ip', str(labels[label][decl][0] - cur)]

def warn_unused_label(label):
    if not labels[label][filename][2] and not (filename == library and labels[label][filename][1]):
        print >> sys.stderr, '{}:{}: warning: unused label \'{}\''.format(filename, pos, label)

def show_label(i):
    if i in rev_labels:
        return '# {}'.format(', '.join(rev_labels[i]))
    return ''


# ----------------------------------------------------------------------
#       main process
# ----------------------------------------------------------------------

# parse command line arguments
argparser = argparse.ArgumentParser(usage='%(prog)s [options] file...')
argparser.add_argument('inputs', nargs='*', help='input files', metavar='files...')
argparser.add_argument('-l', help='set library file to <file> (default: {})'.format(library), metavar='<file>')
argparser.add_argument('-o', help='set output file to <file>', metavar='<file>')
argparser.add_argument('-s', action='store_const', const=True, help='output primitive assembly')
argparser.add_argument('-k', action='store_const', const=True, help='output as array of std_logic_vector format')
argparser.add_argument('-a', action='store_const', const=True, help='output as rs232c send test format')
args = argparser.parse_args()
if args.inputs == []:
    prog = re.sub(r'.*[/\\]', '', sys.argv[0])
    print >> sys.stderr, 'usage: {} [options] file...'.format(prog)
    print >> sys.stderr, ''
    print >> sys.stderr, '  -l\tset library file to <file> (default: {})'.format(library)
    print >> sys.stderr, '  -o\tset output file to <file>'
    print >> sys.stderr, '  -s\toutput primitive assembly'
    print >> sys.stderr, ''
    print >> sys.stderr, '{}: error: no input files'.format(prog)
    sys.exit(1)
if args.l:
    library = args.l
if os.path.isfile(library) and library not in args.inputs:
    args.inputs.append(library)
library = re.sub(r'.*[/\\]', '', library)

# 0. preprocess
lines0 = [('nop', '', 0), ('br main', '_main', 0)]
for filename in args.inputs:
    with open(filename, 'r') as f:
        filename = re.sub(r'.*[/\\]', '', filename)
        srcs[filename] = {}
        for pos, line in enumerate(f):
            line = line.strip()
            srcs[filename][pos + 1] = line
            line = re.sub(r'[;#].*', '', line).strip()
            if line:
                lines0.append((line, filename, pos + 1))
lines0.extend([('halt', '', 0)] * 3)

# 1. macro expansion
lines1 = []
for line, filename, pos in lines0:
    mnemonic, operands = parse(line)
    if mnemonic in macro_table:
        lines = macro_table[mnemonic](operands)
        lines1.extend(map(lambda x: (x, filename, pos), lines))
    else:
        lines1.append((line, filename, pos))

# 2. label resolution (by 2-pass algorithm)
i = 0
lines2 = []
lines3 = []
for line, filename, pos in lines1:
    mnemonic, operands = parse(line)
    if mnemonic[-1] == ':':
        if len(operands) > 0:
            error('invalid syntax')
        add_label(line[:-1], i)
    elif mnemonic == '.global':
        check_operands_n(operands, 1)
        add_global(operands[0])
    else:
        lines2.append((line, filename, pos))
        i += 1
for i, (line, filename, pos) in enumerate(lines2):
    mnemonic, operands = parse(line)
    if mnemonic not in table:
        error('unknown mnemonic \'{}\''.format(mnemonic))
    check_operands_n(operands, 1, 4)
    operands[-1:] = subst(operands[-1], i)
    lines3.append(('{} {}'.format(mnemonic, ', '.join(operands)), filename, pos))
for line, filename, pos in lines1:
    mnemonic, operands = parse(line)
    if mnemonic[-1] == ':':
        warn_unused_label(line[:-1])
    if mnemonic == '.global':
        check_global(operands[0])

# 3. assemble
if not args.o:
    m = re.match(r'(.*)\.', args.inputs[0])
    args.o = '{}.out'.format(m.group(1) if m else args.inputs[0])
with open(args.o, 'w') as f:
    for i, (line, filename, pos) in enumerate(lines3):
        mnemonic, operands = parse(line)
        byterepr = table[mnemonic](operands)
        if args.k:
            f.write("{} => x\"{}\",\n".format(i, ''.join('{:02x}'.format(ord(x)) for x in byterepr)))
        elif args.a:
            fmt = """
            wait for BR; RS_RX <= '0';
            wait for BR; RS_RX <= '{}';
            wait for BR; RS_RX <= '{}';
            wait for BR; RS_RX <= '{}';
            wait for BR; RS_RX <= '{}';
            wait for BR; RS_RX <= '{}';
            wait for BR; RS_RX <= '{}';
            wait for BR; RS_RX <= '{}';
            wait for BR; RS_RX <= '{}';
            wait for BR; RS_RX <= '1';

            wait for (2 * BR);

            """
            for b in byterepr:
                a = ord(b)
                ps = ['1' if a & (1 << j) else '0' for j in range(8)]
                f.write(fmt.format(*ps))
        else:
            f.write(byterepr)
    if args.k:
        f.write("others => (others => '0')\n")
if args.s:
    with open(args.o + '.s', 'w') as f:
        for i, (line, filename, pos) in enumerate(lines3):
            mnemonic, operands = parse(line)
            f.write('{:7} {:19} {}'.format(mnemonic, ', '.join(operands), show_label(i)).strip() + '\n')
