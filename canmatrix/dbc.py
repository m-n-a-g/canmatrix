#!/usr/bin/env python
# Copyright (c) 2013, Eduard Broecker
# All rights reserved.
#
# Redistribution and use in source and binary forms, with or without modification, are permitted provided that
# the following conditions are met:
#
#    Redistributions of source code must retain the above copyright notice, this list of conditions and the
#    following disclaimer.
#    Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the
#    following disclaimer in the documentation and/or other materials provided with the distribution.
#
# THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED
# WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A
# PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY
# DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
# PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
# CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR
# OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH
# DAMAGE.

#
# this script exports dbc-files from a canmatrix-object
# dbc-files are the can-matrix-definitions of the CanOe (Vector Informatic)
from __future__ import division
from __future__ import print_function
from __future__ import absolute_import

from operator import attrgetter, itemgetter, methodcaller
import collections
import logging
logger = logging.getLogger('root')

from builtins import *
import math
from .canmatrix import *
import re
import codecs


#dbcExportEncoding = 'iso-8859-1'
# CP1253


def normalizeName(name, whitespaceReplacement):
    name = re.sub('\s+', whitespaceReplacement, name)

    if ' ' in name:
        name = '"' + name + '"'

    return name


def format_float(f):
    s = str(f).upper()
    if s.endswith('.0'):
        s = s[:-2]

    if 'E' in s:
        s = s.split('E')
        s = '%sE%s%s' % (s[0], s[1][0], s[1][1:].rjust(3, '0'))

    return s.upper()


def dump(db, f, **options):
    if 'dbcExportEncoding' in options:
        dbcExportEncoding = options["dbcExportEncoding"]
    else:
        dbcExportEncoding = 'iso-8859-1'
    if 'dbcExportCommentEncoding' in options:
        dbcExportCommentEncoding = options["dbcExportCommentEncoding"]
    else:
        dbcExportCommentEncoding = dbcExportEncoding
    if 'whitespaceReplacement' in options:
        whitespaceReplacement = options["whitespaceReplacement"]
        if whitespaceReplacement in ['', None] or set(
                [' ', '\t']).intersection(whitespaceReplacement):
            print("Warning: Settings may result in whitespace in DBC variable names.  This is not supported by the DBC format.")
    else:
        whitespaceReplacement = '_'
    if 'writeValTable' in options:
        writeValTable = options["writeValTable"]
    else:
        writeValTable = True
    if 'dbcExportSorted' in options:
        if True == options["dbcExportSorted"]:
            dbFrames = sorted(db.frames, key=attrgetter('id'))
        else:
            dbFrames = db.frames
    else:
        dbFrames = db.frames

    f.write("VERSION \"created by canmatrix\"\n\n".encode(dbcExportEncoding))
    f.write("\n".encode(dbcExportEncoding))

    f.write("NS_ :\n".encode(dbcExportEncoding))
    for ns, val in db.NewSymbols.items():
        f.write(('\t' + val + '\n').encode(dbcExportEncoding))
    f.write("\n".encode(dbcExportEncoding))
    f.write("BS_:\n\n".encode(dbcExportEncoding))

    # Boardunits
    f.write("BU_: ".encode(dbcExportEncoding))
    id = 1
    nodeList = {}
    for bu in db.boardUnits:
        f.write((bu.name + " ").encode(dbcExportEncoding))
    f.write("\n\n".encode(dbcExportEncoding))

    if writeValTable:
        # ValueTables
        for table in db.valueTables:
            f.write(("VAL_TABLE_ " + table).encode(dbcExportEncoding))
            for row in db.valueTables[table]:
                f.write(
                    (' ' +
                     str(row) +
                     ' "' +
                     db.valueTables[table][row] +
                     '"').encode(dbcExportEncoding))
            f.write(";\n".encode(dbcExportEncoding))
        f.write("\n".encode(dbcExportEncoding))

    output_names = collections.defaultdict(dict)

    for frame in db.frames:
        normalized_names = collections.OrderedDict((
            (s, normalizeName(s.name, whitespaceReplacement))
            for s in frame.signals
        ))

        duplicate_signal_totals = collections.Counter(normalized_names.values())
        duplicate_signal_counter = collections.Counter()

        numbered_names = collections.OrderedDict()

        for signal in frame.signals:
            name = normalized_names[signal]
            duplicate_signal_counter[name] += 1
            if duplicate_signal_totals[name] > 1:
                # TODO: pad to 01 in case of 10+ instances, for example?
                name += str(duplicate_signal_counter[name] - 1)

            output_names[frame][signal] = name

    # Frames
    for bo in dbFrames:
        multiplex_written = False
        if bo.transmitter.__len__() == 0:
            bo.addTransmitter("Vector__XXX")

        if bo.extended == 1:
            bo.id += 0x80000000
        
        f.write(
            ("BO_ %d " %
             bo.id +
             bo.name +
             ": %d " %
             bo.size +
             bo.transmitter[0] +
             "\n").encode(dbcExportEncoding))
        duplicate_signal_totals = collections.Counter(
            normalizeName(s.name, whitespaceReplacement) for s in bo.signals
        )
        duplicate_signal_counter = collections.Counter()
        if True == options["dbcExportSorted"]:
            boSignals = sorted(bo.signals, key=methodcaller('getStartbit',bitNumbering=1))
        else:
            boSignals = bo.signals

        for signal in boSignals:
            if signal.multiplex == 'Multiplexor' and multiplex_written:
                continue

            f.write((" SG_ " + output_names[bo][signal]).encode(dbcExportEncoding))
            if signal.multiplex == 'Multiplexor':
                f.write(' M '.encode(dbcExportEncoding))
                multiplex_written = True
            elif signal.multiplex is not None:
                f.write((" m%d " %
                         int(signal.multiplex)).encode(dbcExportEncoding))

            startbit = signal.getStartbit(bitNumbering=1)

            if signal.is_signed:
                sign = '-'
            else:
                sign = '+'
            f.write(
                (" : %d|%d@%d%c" %
                 (startbit,
                  signal.signalsize,
                  signal.is_little_endian,
                  sign)).encode(dbcExportEncoding))
            f.write(
                (" (%s,%s)" %
                 (format_float(signal.factor), format_float(signal.offset))).encode(dbcExportEncoding))
            f.write(
                (" [{}|{}]".format(
                    format_float(signal.min),
                    format_float(signal.max))).encode(dbcExportEncoding))
            f.write(' "'.encode(dbcExportEncoding))

            if signal.unit is None:
                signal.unit = ""
            f.write(signal.unit.encode(dbcExportEncoding))
            f.write('" '.encode(dbcExportEncoding))
            if signal.receiver.__len__() == 0:
                signal.addReceiver('Vector__XXX')
            f.write((','.join(signal.receiver) + "\n").encode(dbcExportEncoding))
        f.write("\n".encode(dbcExportEncoding))
    f.write("\n".encode(dbcExportEncoding))

    # second Sender:
    for bo in dbFrames:
        if bo.transmitter.__len__() > 1:
            f.write(
                ("BO_TX_BU_ %d : %s;\n" %
                 (bo.id, ','.join(
                     bo.transmitter))).encode(dbcExportEncoding))

    # frame comments
    for bo in dbFrames:
        if bo.comment is not None and bo.comment.__len__() > 0:
            f.write(
                ("CM_ BO_ " +
                 "%d " %
                 bo.id +
                 '"').encode(dbcExportEncoding))
            f.write(
                bo.comment.replace(
                    '"',
                    '\\"').encode(dbcExportCommentEncoding, 'ignore'))
            f.write('";\n'.encode(dbcExportEncoding))
            if False == options["dbcExportSorted"]:
                    for signal in bo.signals:
                        if signal.comment is not None and signal.comment.__len__() > 0:
                            name = output_names[bo][signal]
                            f.write(
                                ("CM_ SG_ " +
                                 "%d " %
                                 bo.id +
                                name +
                                 ' "').encode(dbcExportEncoding, 'ignore'))
                            f.write(
                                   signal.comment.replace(
                                        '"', '\\"').encode(dbcExportCommentEncoding, 'ignore'))
                            f.write('";\n'.encode(dbcExportEncoding, 'ignore'))
        elif bo.comment is None and False == options["dbcExportSorted"]:
            for signal in bo.signals:
                if signal.comment is not None and signal.comment.__len__() > 0:
                    name = output_names[bo][signal]
                    f.write(
                        ("CM_ SG_ " +
                         "%d " %
                         bo.id +
                         name +
                         ' "').encode(dbcExportEncoding, 'ignore'))
                    f.write(
                            signal.comment.replace(
                                '"', '\\"').encode(dbcExportCommentEncoding, 'ignore'))
                    f.write('";\n'.encode(dbcExportEncoding, 'ignore'))

    f.write("\n".encode(dbcExportEncoding))

    # signal comments
    if True == options["dbcExportSorted"]:
        for bo in dbFrames:
            for signal in bo.signals:
                if signal.comment is not None and signal.comment.__len__() > 0:
                    name = output_names[bo][signal]
                    f.write(
                        ("CM_ SG_ " +
                         "%d " %
                         bo.id +
                         name +
                         ' "').encode(dbcExportEncoding, 'ignore'))
                    f.write(
                            signal.comment.replace(
                                '"', '\\"').encode(dbcExportCommentEncoding, 'ignore'))
                    f.write('";\n'.encode(dbcExportEncoding, 'ignore'))

    f.write("\n".encode(dbcExportEncoding))

    # boardUnit comments
    for bu in db.boardUnits:
        if bu.comment is not None and bu.comment.__len__() > 0:
            f.write(
                ("CM_ BU_ " +
                 bu.name +
                 ' "' +
                 bu.comment.replace(
                     '"',
                     '\\"') +
                    '";\n').encode(dbcExportCommentEncoding,'ignore'))
    f.write("\n".encode(dbcExportEncoding))

    defaults = {}
    if True == options["dbcExportSorted"]:
        sigDefines = sorted(list(db.signalDefines.items()))
    else:
        sigDefines = list(db.signalDefines.items())
    for (type, define) in sigDefines:
        f.write(
            ('BA_DEF_ SG_ "' +
             type +
             '" ').encode(dbcExportEncoding) +
            define.definition.encode(
                dbcExportEncoding,
                'replace') +
            ';\n'.encode(dbcExportEncoding))
        if type not in defaults and define.defaultValue is not None:
            defaults[type] = define.defaultValue

    if True == options["dbcExportSorted"]:
        frDefines = sorted(list(db.frameDefines.items()))
    else:
        frDefines = list(db.frameDefines.items())
    for (type, define) in frDefines:
        f.write(
            ('BA_DEF_ BO_ "' +
             type +
             '" ').encode(dbcExportEncoding) +
            define.definition.encode(
                dbcExportEncoding,
                'replace') +
            ';\n'.encode(dbcExportEncoding))
        if type not in defaults and define.defaultValue is not None:
            defaults[type] = define.defaultValue

    if True == options["dbcExportSorted"]:
        ecuDefines = sorted(list(db.buDefines.items()))
    else:
        ecuDefines = list(db.buDefines.items())
    for (type, define) in ecuDefines:
        f.write(
            ('BA_DEF_ BU_ "' +
             type +
             '" ').encode(dbcExportEncoding) +
            define.definition.encode(
                dbcExportEncoding,
                'replace') +
            ';\n'.encode(dbcExportEncoding))
        if type not in defaults and define.defaultValue is not None:
            defaults[type] = define.defaultValue

    if True == options["dbcExportSorted"]:
        netDefines = sorted(list(db.globalDefines.items()))
    else:
        netDefines = list(db.globalDefines.items())
    for (type, define) in netDefines:
        f.write(
            ('BA_DEF_ "' +
             type +
             '" ').encode(dbcExportEncoding) +
            define.definition.encode(
                dbcExportEncoding,
                'replace') +
            ';\n'.encode(dbcExportEncoding))
        if type not in defaults and define.defaultValue is not None:
            defaults[type] = define.defaultValue

    if True == options["dbcExportSorted"]:
        defDefines = sorted(defaults)
    else:
        defDefines = defaults
    for define in defDefines:
        f.write(
            ('BA_DEF_DEF_ "' +
             define +
             '" ').encode(dbcExportEncoding) +
            defaults[define].encode(
                dbcExportEncoding,
                'replace') +
            ';\n'.encode(dbcExportEncoding))

    # boardunit-attributes:
    for bu in db.boardUnits:
        if True == options["dbcExportSorted"]:
            ecuAttribs = sorted(bu.attributes.items())
        else:
            ecuAttribs = bu.attributes.items()
        for attrib, val in ecuAttribs:
            if db.buDefines[attrib].type == "STRING":
                val = '"' + val + '"'
            elif not val:
                val = '""'
            f.write(
                ('BA_ "' +
                 attrib +
                 '" BU_ ' +
                 bu.name +
                 ' ' +
                 str(val) +
                    ';\n').encode(dbcExportEncoding))
    f.write("\n".encode(dbcExportEncoding))

    # global-attributes:
    if True == options["dbcExportSorted"]:
        netAttribs = sorted(db.attributes.items())
    else:
        netAttribs = db.attributes.items()
    for attrib, val in netAttribs:
        if db.globalDefines[attrib].type == "STRING":
            val = '"' + val + '"'
        elif not val:
            val = '""'
        f.write(('BA_ "' + attrib + '" ' + val +
                 ';\n').encode(dbcExportEncoding))
    f.write("\n".encode(dbcExportEncoding))

    # messages-attributes:
    for frame in dbFrames:
        if True == options["dbcExportSorted"]:
            frAttribs = sorted(frame.attributes.items())
        else:
            frAttribs = frame.attributes.items()
        for attrib, val in frAttribs:
            if db.frameDefines[attrib].type == "STRING":
                val = '"' + val + '"'
            elif not val:
                val = '""'
            f.write(('BA_ "' + attrib + '" BO_ %d ' %
                     frame.id + val + ';\n').encode(dbcExportEncoding))
    f.write("\n".encode(dbcExportEncoding))

    # signal-attributes:
    for frame in dbFrames:
        for signal in frame.signals:
            if True == options["dbcExportSorted"]:
                sigAttribs = sorted(signal.attributes.items())
            else:
                sigAttribs = signal.attributes.items()
            for attrib, val in sigAttribs:
                name = output_names[frame][signal]
                if db.signalDefines[attrib].type == "STRING":
                    val = '"' + val + '"'
                elif not val:
                    val = '""'
                elif isinstance(val, float):
                    val = format_float(val)
                f.write(
                    ('BA_ "' +
                     attrib +
                     '" SG_ %d ' %
                     frame.id +
                     name +
                     ' ' +
                     val +
                     ';\n').encode(dbcExportEncoding))
            if signal.is_float:
                if int(signal.signalsize) > 32:
                    f.write(('SIG_VALTYPE_ %d %s : 2;\n' % (frame.id, output_names[bo][signal])).encode(dbcExportEncoding))
                else:
                    f.write(('SIG_VALTYPE_ %d %s : 1;\n' % (frame.id, output_names[bo][signal])).encode(dbcExportEncoding))
 
    f.write("\n".encode(dbcExportEncoding))

    # signal-values:
    for bo in dbFrames:
        multiplex_written = False
        for signal in bo.signals:
            if signal.multiplex == 'Multiplexor' and multiplex_written:
                continue

            multiplex_written = True

            if signal.values:
                f.write(
                    ('VAL_ %d ' %
                     bo.id +
                     output_names[bo][signal]).encode(dbcExportEncoding))
                for attrib, val in sorted(
                        signal.values.items(), key=lambda x: int(x[0])):
                    f.write(
                        (' ' + str(attrib) + ' "' + val + '"').encode(dbcExportEncoding))
                f.write(";\n".encode(dbcExportEncoding))

    # signal-groups:
    for bo in dbFrames:
        for sigGroup in bo.SignalGroups:
            f.write(("SIG_GROUP_ " + str(bo.id) + " " + sigGroup.name +
                     " " + str(sigGroup.id) + " :").encode(dbcExportEncoding))
            for signal in sigGroup.signals:
                f.write((" " + output_names[bo][signal]).encode(dbcExportEncoding))
            f.write(";\n".encode(dbcExportEncoding))


def load(f, **options):
    if 'dbcImportEncoding' in options:
        dbcImportEncoding = options["dbcImportEncoding"]
    else:
        dbcImportEncoding = 'iso-8859-1'
    if 'dbcImportCommentEncoding' in options:
        dbcCommentEncoding = options["dbcImportCommentEncoding"]
    else:
        dbcCommentEncoding = dbcImportEncoding

    i = 0
    NS = 0

    class FollowUps(object):
        nothing, signalComment, frameComment, boardUnitComment, globalComment = list(
            range(5))
    followUp = FollowUps.nothing
    comment = ""
    signal = None
    frame = None
    boardUnit = None
    db = CanMatrix()
    for line in f:
        i = i + 1
        l = line.strip()

        if followUp == FollowUps.signalComment:
            try:
                comment += "\n" + \
                    l.decode(dbcCommentEncoding).replace('\\"', '"').replace(u"\u2018", u"\u0027").replace(u"\u2019", u"\u0027")
            except:
                logger.error("Error decoding line: %d (%s)" % (i, line))
            if l.endswith(b'";'):
                followUp = FollowUps.nothing
                if signal is not None:
                    signal.addComment(comment[0:-2])
            continue
        elif followUp == FollowUps.frameComment:
            try:
                comment += "\n" + \
                    l.decode(dbcCommentEncoding).replace('\\"', '"').replace(u"\u2018", u"\u0027").replace(u"\u2019", u"\u0027")
            except:
                logger.error("Error decoding line: %d (%s)" % (i, line))
            if l.endswith(b'";'):
                followUp = FollowUps.nothing
                if frame is not None:
                    frame.addComment(comment[0:-2])
            continue
        elif followUp == FollowUps.boardUnitComment:
            try:
                comment += "\n" + \
                    l.decode(dbcCommentEncoding).replace('\\"', '"').replace(u"\u2018", u"\u0027").replace(u"\u2019", u"\u0027")
            except:
                logger.error("Error decoding line: %d (%s)" % (i, line))
            if l.endswith(b'";'):
                followUp = FollowUps.nothing
                if boardUnit is not None:
                    boardUnit.addComment(comment[0:-2])
            continue

        if l.__len__() == 0:
            continue
        decoded = l.decode(dbcImportEncoding).replace(u"\u2018", u"\u0027").replace(u"\u2019", u"\u0027")
        if decoded.startswith("BO_ "):
            regexp = re.compile("^BO\_ (\w+) (\w+) *: (\w+) (\w+)")
            temp = regexp.match(decoded)
#            db._fl.addFrame(Frame(temp.group(1), temp.group(2), temp.group(3), temp.group(4)))
            db._fl.addFrame(Frame(temp.group(2),
                                  Id=temp.group(1),
                                  dlc=temp.group(3),
                                  transmitter=temp.group(4).split()))
        elif decoded.startswith("SG_ "):
            pattern = r"^SG\_ (\w+) : (\d+)\|(\d+)@(\d+)([\+|\-]) \(([0-9.+\-eE]+),([0-9.+\-eE]+)\) ?\[([0-9.+\-eE]+)\|([0-9.+\-eE]+)\] \"(.*)\" (.*)"
            regexp = re.compile(pattern)
            temp = regexp.match(decoded)
            regexp_raw = re.compile(pattern.encode(dbcImportEncoding))
            temp_raw = regexp_raw.match(l)
            if temp:
                receiver = list(map(str.strip, temp.group(11).split(',')))

                tempSig = Signal(temp.group(1),
                                 startBit=temp.group(2),
                                 signalSize=temp.group(3),
                                 is_little_endian=(int(temp.group(4)) == 1),
                                 is_signed=(temp.group(5) == '-'),
                                 factor=temp.group(6),
                                 offset=temp.group(7),
                                 min=temp.group(8),
                                 max=temp.group(9),
                                 unit=temp_raw.group(10).decode(
                                     dbcImportEncoding),
                                 receiver=receiver)
                if not tempSig.is_little_endian:
                    # startbit of motorola coded signals are MSB in dbc
                    tempSig.setStartbit(int(temp.group(2)), bitNumbering=1)
                db._fl.addSignalToLastFrame(tempSig)
            else:
                pattern = r"^SG\_ (\w+) (\w+) *: (\d+)\|(\d+)@(\d+)([\+|\-]) \(([0-9.+\-eE]+),([0-9.+\-eE]+)\) ?\[([0-9.+\-eE]+)\|([0-9.+\-eE]+)\] \"(.*)\" (.*)"
                regexp = re.compile(pattern)
                regexp_raw = re.compile(pattern.encode(dbcImportEncoding))
                temp = regexp.match(decoded)
                temp_raw = regexp_raw.match(l)
                receiver = list(map(str.strip, temp.group(12).split(',')))
                multiplex = temp.group(2)
                if multiplex == 'M':
                    multiplex = 'Multiplexor'
                else:
                    multiplex = int(multiplex[1:])
                tempSig = Signal(temp.group(1),
                                 startBit=temp.group(3),
                                 signalSize=temp.group(4),
                                 is_little_endian=(int(temp.group(5)) == 1),
                                 is_signed=(temp.group(6) == '-'),
                                 factor=temp.group(7),
                                 offset=temp.group(8),
                                 min=temp.group(9),
                                 max=temp.group(10),
                                 unit=temp_raw.group(11).decode(
                                     dbcImportEncoding),
                                 receiver=receiver,
                                 multiplex=multiplex)
                if not tempSig.is_little_endian:
                    # startbit of motorola coded signals are MSB in dbc
                    tempSig.setStartbit(int(temp.group(3)), bitNumbering=1)

                db._fl.addSignalToLastFrame(tempSig)

        elif decoded.startswith("BO_TX_BU_ "):
            regexp = re.compile(r"^BO_TX_BU_ ([0-9]+) *: *(.+);")
            temp = regexp.match(decoded)
            botschaft = db.frameById(temp.group(1))
            for bu in temp.group(2).split(','):
                botschaft.addTransmitter(bu)
        elif decoded.startswith("CM_ SG_ "):
            pattern = r"^CM\_ SG\_ *(\w+) *(\w+) *\"(.*)\";"
            regexp = re.compile(pattern)
            regexp_raw = re.compile(pattern.encode(dbcImportEncoding))
            temp = regexp.match(decoded)
            temp_raw = regexp_raw.match(l)
            if temp:
                botschaft = db.frameById(temp.group(1))
                signal = botschaft.signalByName(temp.group(2))
                if signal:
                    try:
                        signal.addComment(temp_raw.group(3).decode(
                            dbcCommentEncoding).replace('\\"', '"'))
                    except:
                        logger.error(
                            "Error decoding line: %d (%s)" %
                            (i, line))
            else:
                pattern = r"^CM\_ SG\_ *(\w+) *(\w+) *\"(.*)"
                regexp = re.compile(pattern)
                regexp_raw = re.compile(pattern.encode(dbcImportEncoding))
                temp = regexp.match(decoded)
                temp_raw = regexp_raw.match(l)
                if temp:
                    botschaft = db.frameById(temp.group(1))
                    signal = botschaft.signalByName(temp.group(2))
                    try:
                        comment = temp_raw.group(3).decode(
                            dbcCommentEncoding).replace('\\"', '"')
                    except:
                        logger.error(
                            "Error decoding line: %d (%s)" %
                            (i, line))
                    followUp = FollowUps.signalComment
                else:
                    logger.info(
                        "Warning: Error decoding line: %d (%s)" %
                       (i, line))
        elif decoded.startswith("CM_ BO_ "):
            pattern = r"^CM\_ BO\_ *(\w+) *\"(.*)\";"
            regexp = re.compile(pattern)
            regexp_raw = re.compile(pattern.encode(dbcImportEncoding))
            temp = regexp.match(decoded)
            temp_raw = regexp_raw.match(l)
            if temp:
                frame = db.frameById(temp.group(1))
                if frame:
                    try:
                        frame.addComment(temp_raw.group(2).decode(
                            dbcCommentEncoding).replace('\\"', '"'))
                    except:
                        logger.error(
                            "Error decoding line: %d (%s)" %
                            (i, line))
            else:
                pattern = r"^CM\_ BO\_ *(\w+) *\"(.*)"
                regexp = re.compile(pattern)
                regexp_raw = re.compile(pattern.encode(dbcImportEncoding))
                temp = regexp.match(decoded)
                temp_raw = regexp_raw.match(l)
                if temp:
                    frame = db.frameById(temp.group(1))
                    try:
                        comment = temp_raw.group(2).decode(
                            dbcCommentEncoding).replace('\\"', '"')
                    except:
                        logger.error(
                            "Error decoding line: %d (%s)" %
                            (i, line))
                    followUp = FollowUps.frameComment
                else:
                    logger.info(
                        "Warning: Error decoding line: %d (%s)" %
                       (i, line))
        elif decoded.startswith("CM_ BU_ "):
            pattern = r"^CM\_ BU\_ *(\w+) *\"(.*)\";"
            regexp = re.compile(pattern)
            regexp_raw = re.compile(pattern.encode(dbcImportEncoding))
            temp = regexp.match(decoded)
            temp_raw = regexp_raw.match(l)
            if temp:
                boardUnit = db.boardUnitByName(temp.group(1))
                if boardUnit:
                    try:
                        boardUnit.addComment(temp_raw.group(2).decode(
                            dbcCommentEncoding).replace('\\"', '"'))
                    except:
                        logger.error(
                            "Error decoding line: %d (%s)" %
                            (i, line))
            else:
                pattern = r"^CM\_ BU\_ *(\w+) *\"(.*)"
                regexp = re.compile(pattern)
                regexp_raw = re.compile(pattern.encode(dbcImportEncoding))
                temp = regexp.match(decoded)
                temp_raw = regexp_raw.match(l)
                if temp:
                    boardUnit = db.boardUnitByName(temp.group(1))
                    if boardUnit:
                        try:
                            comment = temp_raw.group(2).decode(
                                dbcCommentEncoding).replace('\\"', '"')
                        except:
                            logger.error(
                                "Error decoding line: %d (%s)" %
                                (i, line))
                        followUp = FollowUps.boardUnitComment
                else:
                    logger.info(
                        "Warning: Error decoding line: %d (%s)" %
                       (i, line))
        elif decoded.startswith("BU_:"):
            pattern = r"^BU\_\:(.*)"
            regexp = re.compile(pattern)
            regexp_raw = re.compile(pattern.encode(dbcImportEncoding))
            temp = regexp.match(decoded)
            if temp:
                myTempListe = temp.group(1).split(' ')
                for ele in myTempListe:
                    if len(ele.strip()) > 1:
                        db._BUs.add(BoardUnit(ele))
            else:
                logger.info(
                    "Warning: Error decoding line: %d (%s)" %
                    (i, line))

        elif decoded.startswith("BS_:"):
            NS = 0

        elif decoded.startswith("NS_ :"):
            NS = 1

        elif NS > 0:
            pattern = r"([\w]+)"
            regexp = re.compile(pattern)
            temp = regexp.match(decoded)
            if temp:
                db.addNewSymbol(NS-1,temp.group(1))
                NS = NS + 1
            else:
                logger.info(
                    "Warning: Error decoding line: %d (%s)" %
                   (i, line))
        elif decoded.startswith("VAL_ "):
            regexp = re.compile(r"^VAL\_ (\w+) (\w+) (.*);")
            temp = regexp.match(decoded)
            if temp:
                botschaftId = temp.group(1)
                signal = temp.group(2)
                tempList = temp.group(3).split('"')
                if botschaftId.isnumeric(): # value for Frame
                    try:
                        bo = db.frameById(botschaftId)
                        sg = bo.signalByName(signal)
                        for i in range(math.floor(len(tempList) / 2)):
                            val = tempList[i * 2 + 1]
                            if sg:
                                sg.addValues(tempList[i * 2], val)
                    except:
                        logger.error("Error with Line: " + str(tempList))
                else:
                    logger.info("Warning: enviroment variables currently not supported")
            else:
                logger.info(
                    "Warning: Error decoding line: %d (%s)" %
                   (i, line))
        elif decoded.startswith("VAL_TABLE_ "):
            regexp = re.compile(r"^VAL\_TABLE\_ (\w+) (.*);")
            temp = regexp.match(decoded)
            if temp:
                tableName = temp.group(1)
                tempList = temp.group(2).split('"')
                try:
                    valHash = {}
                    for i in range(math.floor(len(tempList) / 2)):
                        val = tempList[i * 2 + 1]
                        valHash[tempList[i * 2].strip()] = val.strip()
                except:
                    logger.error("Error with Line: " + str(tempList))
                db.addValueTable(tableName, valHash)
            else:
                logger.debug(l)

        elif decoded.startswith("BA_DEF_ SG_ "):
            pattern = r"^BA\_DEF\_ SG\_ +\"([A-Za-z0-9\-_]+)\" +(.+);"
            regexp = re.compile(pattern)
            regexp_raw = re.compile(pattern.encode(dbcImportEncoding))
            temp = regexp.match(decoded)
            temp_raw = regexp_raw.match(l)
            if temp:
                db.addSignalDefines(temp.group(1),
                                    temp_raw.group(2).decode(dbcImportEncoding))
            else:
                logger.info(
                    "Warning: Error decoding line: %d (%s)" %
                   (i, line))
        elif decoded.startswith("BA_DEF_ BO_ "):
            pattern = r"^BA\_DEF\_ BO\_ +\"([A-Za-z0-9\-_]+)\" +(.+);"
            regexp = re.compile(pattern)
            regexp_raw = re.compile(pattern.encode(dbcImportEncoding))
            temp = regexp.match(decoded)
            temp_raw = regexp_raw.match(l)
            if temp:
                db.addFrameDefines(temp.group(1),
                                   temp_raw.group(2).decode(dbcImportEncoding))
            else:
                logger.info(
                    "Warning: Error decoding line: %d (%s)" %
                   (i, line))
        elif decoded.startswith("BA_DEF_ BU_ "):
            pattern = r"^BA\_DEF\_ BU\_ +\"([A-Za-z0-9\-_]+)\" +(.+);"
            regexp = re.compile(pattern)
            regexp_raw = re.compile(pattern.encode(dbcImportEncoding))
            temp = regexp.match(decoded)
            temp_raw = regexp_raw.match(l)
            if temp:
                db.addBUDefines(temp.group(1),
                                temp_raw.group(2).decode(dbcImportEncoding))
            else:
                logger.info(
                    "Warning: Error decoding line: %d (%s)" %
                   (i, line))
        elif decoded.startswith("BA_DEF_ "):
            pattern = r"^BA\_DEF\_ +\"([A-Za-z0-9\-_]+)\" +(.+);"
            regexp = re.compile(pattern)
            regexp_raw = re.compile(pattern.encode(dbcImportEncoding))
            temp = regexp.match(decoded)
            temp_raw = regexp_raw.match(l)
            if temp:
                db.addGlobalDefines(temp.group(1),
                                    temp_raw.group(2).decode(dbcImportEncoding))
            else:
                logger.info(
                    "Warning: Error decoding line: %d (%s)" %
                   (i, line))
        elif decoded.startswith("BA_ "):
            regexp = re.compile(r"^BA\_ +\"[A-Za-z0-9[-_(-\/ .]+\" +(.+)")
            tempba = regexp.match(decoded)
            if tempba.group(1).strip().startswith("BO_ "):
                regexp = re.compile(r"^BA\_ \"(.*)\" BO\_ (\w+) (.+);")
                temp = regexp.match(decoded)
                db.frameById(int(temp.group(2))).addAttribute(
                    temp.group(1), temp.group(3))
            elif tempba.group(1).strip().startswith("SG_ "):
                regexp = re.compile(r"^BA\_ \"(.*)\" SG\_ (\w+) (\w+) (.+);")
                temp = regexp.match(decoded)
                db.frameById(int(temp.group(2))).signalByName(
                    temp.group(3)).addAttribute(temp.group(1), temp.group(4))
            elif tempba.group(1).strip().startswith("BU_ "):
                regexp = re.compile(r"^BA\_ \"(.*)\" BU\_ (\w+) (.+);")
                temp = regexp.match(decoded)
                db._BUs.byName(
                    temp.group(2)).addAttribute(
                    temp.group(1),
                    temp.group(3))
            else:
                regexp = re.compile(
                r"^BA\_ \"([A-Za-z0-9[-_(-\/ .]+)\" +([\"A-Za-z0-9[-_(-\/ .]+);")
                temp = regexp.match(decoded)
                if temp:
                    db.addAttribute(temp.group(1), temp.group(2))
                else:
                    logger.info(
                       "Warning: Error decoding line: %d (%s)" %
                       (i, line))
        elif decoded.startswith("SIG_GROUP_ "):
            regexp = re.compile(r"^SIG\_GROUP\_ +(\w+) +(\w+) +(\w+) +\:(.*);")
            temp = regexp.match(decoded)
            frame = db.frameById(temp.group(1))
            if frame is not None:
                signalArray = temp.group(4).split(' ')
                frame.addSignalGroup(temp.group(2), temp.group(3), signalArray)
            else:
                logger.info(
                    "Warning: Error decoding line: %d (%s)" %
                   (i, line))
        elif decoded.startswith("SIG_VALTYPE_ "):
            regexp = re.compile(r"^SIG\_VALTYPE\_ +(\w+) +(\w+) +\:(.*);")
            temp = regexp.match(decoded)
            frame = db.frameById(temp.group(1))
            if frame:
                signal = frame.signalByName(temp.group(2))
                signal.is_float = True
#                SIG_VALTYPE_ 0 float : 1;
            else:
                logger.info(
                    "Warning: Error decoding line: %d (%s)" %
                   (i, line))
        elif decoded.startswith("BA_DEF_DEF_ "):
            pattern = r"^BA\_DEF\_DEF\_ +\"([A-Za-z0-9\-_]+)\" +(.+)\;"
            regexp = re.compile(pattern)
            regexp_raw = re.compile(pattern.encode(dbcImportEncoding))
            temp = regexp.match(decoded)
            temp_raw = regexp_raw.match(l)
            if temp:
                db.addDefineDefault(temp.group(1),
                                    temp_raw.group(2).decode(dbcImportEncoding))
            else:
                logger.info(
                    "Warning: Error decoding line: %d (%s)" %
                   (i, line))
#               else:
#                       print "Unrecocniced line: " + l + " (%d) " % i
# Backtracking
    for frame in db.frames:
        # receiver is only given in the signals, so do propagate the receiver
        # to the frame:
        frame.updateReceiver()
        # extended-flag is implicite in canid, thus repair this:
        if frame.id > 0x80000000:
            frame.id -= 0x80000000
            frame.extended = 1
    for define in db.globalDefines:
        if db.globalDefines[define].type == "STRING":
            if define in db.attributes:
                db.attributes[define] = db.attributes[define][1:-1]
    for define in db.buDefines:
        if db.buDefines[define].type == "STRING":
            for ecu in db.boardUnits:
                if define in ecu.attributes:
                    ecu.attributes[define] = ecu.attributes[define][1:-1]
    for define in db.frameDefines:
        if db.frameDefines[define].type == "STRING":
            for frame in db.frames:
                if define in frame.attributes:
                    frame.attributes[define] = frame.attributes[define][1:-1]
    for define in db.signalDefines:
        if db.signalDefines[define].type == "STRING":
            for frame in db.frames:
                for signal in frame.signals:
                    if define in signal.attributes:
                        signal.attributes[define] = signal.attributes[define][1:-1]
    return db
