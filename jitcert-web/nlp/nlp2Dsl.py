#!/usr/bin/env python3.6
# -*- coding: utf-8 -*-

#batteries
import re
import argparse
import socket
import asyncio
from textwrap import dedent, indent
import itertools as it
import logging
import datetime
import sys


# start logger
def setup_logger(time_str):
    FORMAT = '%(levelname) -10s %(asctime)s %(module)s: %(lineno)s %(funcName)s() %(message)s'
    logname = f'{time_str}_nlp.log'
    logging.basicConfig(filename=logname,
                        filemode='w',
                        format=FORMAT,
                        level=logging.DEBUG)
    logger = logging.getLogger(__name__)
    print(f'created log: {logname}', file=sys.stderr)
    return logger


#internal
#import generator
import helper as H

#external
logger = None


class FileError(Exception):
    pass


FILEIN = '_nlp.tmp'


class Policy:
    types = {'PolicyAnd', 'PolicyOr'}

    def __init__(self, s):
        self.s = s

    def haskell(self):
        return self.s


class Goal:

    @classmethod
    def from_generic(cls):
        return cls('<Dummy Goal>')

    def __init__(self, s):
        self.s = s
        self.policy = Policy('PolicyAnd')

    def haskell_defs(self):
        return ''

    def haskell(self):
        s = f'''goal {self.policy.haskell()} [SFText "{self.s}"]'''
        return s


class Strategy:

    @classmethod
    def from_generic(cls):
        return cls('<Dummy Strategy>')

    def __init__(self, s):
        self.s = s

    def haskell_defs(self):
        return ''

    def haskell(self):
        return '<NotImplemented>'


class Contexts:

    @classmethod
    def from_generic(cls):
        return cls([Context.from_generic()])

    def __init__(self, contexts):
        self.contexts = contexts

    def haskell_defs(self):
        #TODO
        return '\n'.join(c.haskell_defs() for c in self.contexts)

    def haskell(self):
        #TODO
        return '\n'.join(f'c{c.cid} <- {c.haskell()}' for c in self.contexts)


class Context:
    _cid = 0

    @classmethod
    def genid(cls):
        cid = cls._cid
        cls._cid += 1
        return cid

    @classmethod
    def from_generic(cls):
        return cls('Dummy_Var_Type', 'Dummy_var_name', '<Dummy description>')

    def __init__(self, vname, vtype=None, description=None):
        self.cid = self.__class__.genid()

        self.vname = H.str2var(vname)
        if vtype is None:
            self.vtype = self.vname.upper()
        else:
            self.vtype = vtype
        if description is None:
            self.description = vname
        else:
            self.description = description

    def haskell_defs(self):
        s1 = (f'''
                data {self.vtype} = {self.vtype} T.Text
                    deriving Eq
               ''')
        s2 = (f'''
                instance RenderContext {self.vtype} where
                    renderContextTypeReference Proxy = "{self.description}"
                    renderContext ({self.vtype} t) = t
               ''')

        return dedent(s1 + s2)

    def haskell(self):

        s = (f'''context @{self.vtype} "{self.vname}" Nothing''')
        return s


class Property:
    property_types = {
        'consistent',
        'include',
        'contain',
    }

    @classmethod
    def from_generic(cls):
        return cls('<Dummy Property>')

    def __init__(self, s='Property-is-true'):
        self.s = s
        return

    def haskell_defs(self):
        s = (f'''
                data Prop = Prop
                instance Property Prop where
                    renderProperty Prop = [SFText "{self.s}"]
                ''')
        return dedent(s)

    def haskell(self):
        return 'Prop'


class Solution:
    _sid = 0

    @classmethod
    def from_generic(cls):
        return cls('<Dummy Solution>')

    solution_types = {
        'inspection',
        'documentation',
        'test',
        'static analysis',
        'dynamic analysis',
    }

    def __init__(self, s):
        self.__class__._sid += 1
        self.sid = self.__class__._sid
        self.s = s
        self.prop = Property()

    def haskell_defs(self):
        return self.prop.haskell_defs() + ''

    def haskell(self):
        '''
        data GenericEvidence c p = GenericEvidence {
              info :: T.Text
            , evidenceValidates :: Bool
            }
            deriving (Eq)

        data GenericSoln c p = GenericSoln (Node c Context) p
        instance (Typeable c, Typeable p, RenderContext c, Property p) => SolutionType (GenericSoln c p) where
            type SolutionEvidence (GenericSoln c p) = GenericEvidence c p

            validEvidence GenericEvidence {..} = evidenceValidates

            renderEvidence GenericEvidence{..} = "GenericEvidence " <> info <> if evidenceValidates then " passed" else " failed"

            renderSolution (GenericSoln c p) = [SFText "GenericSoln", SFContext c, SFProperty p]

        genericSoln
            :: (RenderContext c, Typeable c, Typeable p, Property p, Monad m)
            => Node c Context
            -> p
            -> BuilderT m (Node (GenericSoln c p) Solution)
        genericSoln c p =
            solution (GenericSoln c p)
            '''
        return '''manualInspection'''


class Evidence:

    @classmethod
    def from_generic(cls):
        return cls()

    evidence_types = {
        'report', 'document', 'certificate', 'test result', 'test report',
        'inspection'
    }

    def __init__(self):
        pass


SnEvidence_pairs = {
    ('test', 'report'),
    ('static analysis', 'report'),
    ('static analysis', 'certificate'),
    ('inspection', 'report'),
    ('inspection', 'certificate'),
}


def tohaskell_simple(goal, context, strategy, solution):
    return dedent(f'''
            import JitCert.Nlp2Dsl
            import           JitCert.GSN.Types
            import           JitCert.GSN.Builder
            import           JitCert.Properties
            import           JitCert.Solution
            import           JitCert.GSN.Combinators
            import           JitCert.FIPS.Combinators
            import           JitCert.Context
            import           Data.Proxy
            import           Type.Reflection
            import           JitCert.GSN.Builder.Internal   ( runBuilderT )
            import           JitCert.DotBackend

            import qualified Data.Text.Lazy                as T
            import qualified System.Process                as S

            gsn "{goal}" "{context}" "{strategy}" "{solution}"

            ''')


def tohaskell(goal, contexts, solutions):

    solution_haskell = '\n'.join(
        f'sn{s.sid} <- {s.haskell()} c0 {s.prop.haskell()}' for s in solutions)
    solution_defs = '\n'.join(s.haskell_defs() for s in solutions)

    goal_ctx_edges = '\n'.join(f'g ~> c{c.cid}' for c in contexts.contexts)
    goal_sn_edges = '\n'.join(f'g ~> sn{s.sid}' for s in solutions)
    '''
      let t = utctime (modifiedjulianday 10) 0
      setsolutionevidence sn $ inspected "joe" t true -- (documented t "doc_file")
    '''

    edge_def = dedent('''
    (~>)
      :: (Monad m, ValidEdge t1 t2)
      => Node c t1
      -> Node d t2
      -> BuilderT m (Node c t1)
    (~>) source sink = addEdge source sink >> return source
    ''')

    header = (
        f'''{{-# LANGUAGE TypeApplications #-}}\n'''
        f'''module WebGSN where\n'''
        f'''\n'''
        f'''--import JitCert.Nlp2Dsl\n'''
        f'''import           JitCert.GSN.Types\n'''
        f'''import           JitCert.GSN.Builder\n'''
        f'''import           JitCert.Properties\n'''
        f'''import           JitCert.Solution\n'''
        f'''import           JitCert.GSN.Combinators\n'''
        f'''import           JitCert.FIPS.Combinators\n'''
        f'''import           JitCert.Context\n'''
        f'''import           Data.Proxy\n'''
        f'''import           Type.Reflection\n'''
        f'''import           JitCert.GSN.Builder.Internal   ( runBuilderT)\n'''
        f'''import           JitCert.DotBackend\n'''
        f'''import qualified Data.Text.Lazy                as T\n'''
        f'''import qualified System.Process                as S\n'''
        f'''{edge_def}'''
        f'''\n'''
        f'''{goal.haskell_defs()}\n'''
        f'''\n'''
        f'''{contexts.haskell_defs()}\n'''
        f'''\n'''
        f'''{solution_defs}\n'''
        f'''\n'''
        f'''webGSN :: (GSNGraph, GSNEnv)\n'''
        f'''webGSN = runBuilder $ do\n''')

    doblock = (f'''\n'''
               f'''g <- {goal.haskell()}\n'''
               f'''\n'''
               f'''{contexts.haskell()}\n'''
               f'''{solution_haskell}\n'''
               f'''\n'''
               f'''{goal_ctx_edges}\n'''
               f'''{goal_sn_edges}\n'''
               f'''\n'''
               f'''return ()\n''')

    return header + indent(doblock, '    ')


#gsn "{goal}" "{context}" "{strategy}" "{solution}"


def regex(keyword):
    return r'\b' + f'{keyword}' + r':(.*?)(\b\w+:.*|$)'


def input_from_file():
    try:
        with open(FILEIN, 'r') as fp:
            data = fp.read()
            return data
    except (OSError, IOError) as e:
        raise FileError(e)


def input_from_stdin():
    data = []

    while True:
        try:
            data.append(input())
        except EOFError:
            break
        except KeyboardInterrupt:
            break

    return '\n'.join(data)


def process_input(data):

    # TODO: Fuzzy match in keywords
    goal = re.search(regex('goal'), data, re.IGNORECASE | re.MULTILINE)
    context = re.search(regex('context'), data, re.IGNORECASE | re.MULTILINE)
    #strategy = re.search(regex('strategy'), data, re.IGNORECASE).groups()[0]
    solution = re.search(regex('solution'), data, re.IGNORECASE | re.MULTILINE)

    ret = [
        i.groups()[0].strip() if i is not None else None
        for i in (goal, context, solution)
    ]
    return ret


def gsn2dsl(goal, context, solution):
    dslConstruct = tohaskell(goal, context, solution)
    return dslConstruct


def process_request(req):
    return '', ''


def start_server():
    HOST = 'localhost'
    PORT = 60000

    async def handler(reader, writer):
        data = await reader.read(100)
        message = data.decode()
        addr = writer.get_extra_info('peername')

        print(f"Received {message!r} from {addr!r}")

        print(f"Send: {message!r}")
        writer.write(data)
        await writer.drain()

        print("Close the connection")
        writer.close()

    async def server():
        server = await asyncio.start_server(handler, HOST, PORT)

        addr = server.sockets[0].getsockname()
        print(f'Serving on {addr}')

        async with server:
            await server.serve_forever()

    asyncio.run(server())


def start_server_ll():

    HOST = 'localhost'  #'127.0.0.1'  # Standard loopback interface address (localhost)
    PORT = 60000  # Port to listen on (non-privileged ports are > 1023)

    with socket.socket(socket.AF_INET, socket.SOCK_STREAM) as s:
        s.bind((HOST, PORT))
        s.listen()
        conn, addr = s.accept()
        with conn:
            print('Server up and connected to: ', addr)

            run = True
            while run:

                # get data
                req = ''
                while True:
                    data = conn.recv(1024)
                    if not data:
                        break
                    req += data

                reply, run = process_request(data)
                conn.sendall(reply)

            #TODO cleanup!!
            conn.close()
            exit()


def load_entities():
    CONTEXTFILE = './nlp/fipsig.entities'
    with open(CONTEXTFILE, 'r') as fp:
        context_text = set(fp.read().lower().split('\n'))
    return context_text


def flatten1(x):
    return it.chain(*x)


def parse_para(para):

    def find_goal(para):
        goalS = []
        for sent in H.sentences(para):
            req = H.requirement(sent)
            logger.debug(req)
            if req:
                goalS.append(sent.text)

        if len(goalS) == 0:
            logger.debug('Could not guess a requirement from the text.')
            goal = Goal.from_generic()

        elif len(goalS) == 1:
            goal = Goal(''.join(goalS))
            logger.debug(f'{goal}')

        else:  # len(req) > 1:
            logger.debug(
                f'Text has possibly multiple requirements (ambiguous:{req}).'
            )
            goal = Goal(''.join(goalS))

        return goal

    def find_context(text):
        #tokens = set(H.tokenize(text))
        fipsig_entites = load_entities()
        ctxs = H.fuzzymatch_np(fipsig_entites, text)
        logger.debug(f'find_context: {ctxs}')
        if ctxs:
            return Contexts([Context(c) for c in ctxs])
        else:
            return Contexts.from_generic()

    def find_property(para):
        raise NotImplementedError

    def find_solution(para):
        d = {}
        for st in Solution.solution_types:
            lemmas = H.lemmatize_noun(st)
            for i in lemmas:
                d[i] = st
        sntypes_lemma = d.keys()
        sntypes_lemma_in_para = set(
            flatten1([
                H.fuzzyintersect(sntypes_lemma, sent)
                for sent in H.sentences(para)
            ]))

        st_found = [d[i] for i in sntypes_lemma_in_para]
        logger.debug(f'find_solution: {st_found}')

        return [Solution(st) for st in st_found]

    def find_evidence(para):
        d = {}
        for et in Evidence.evidence_types:
            lemmas = H.lemmatize_noun(et)
            for i in lemmas:
                d[i] = et
        etypes_lemma = d.keys()
        etypes_lemma_in_para = set(
            flatten1([
                H.fuzzyintersect(etypes_lemma, sent)
                for sent in H.sentences(para)
            ]))
        ret = [d[i] for i in etypes_lemma_in_para]
        logger.debug(f'find_evidence: {ret}')
        return ret

    def validate_snev(sns, evs):
        pass

    def get_evs(sns):
        pass

    def get_sns(evs):
        pass

    para = H.cleanup_input(para)

    def find_property():
        pass

    try:
        goal = find_goal(para)
    except Exception:
        logger.exception('Failed to find a requirement/goal.')
        goal = Goal.from_generic()

    try:
        context = find_context(para)
    except Exception:
        logger.exception('Failed to find contexts.')
        context = Context.from_generic()

    try:
        sns = find_solution(para)
    except Exception:
        logger.exception('failed to find solutions.')
        sns = [Solution.from_generic()]

    try:
        evs = find_evidence(para)
    except Exception:
        logger.exception('failed to find evidences.')
        evs = [Evidence.from_generic()]

    #tokens = H.tokenize(para)

    if sns and evs:
        validate_snev(sns, evs)
    elif sns and not evs:
        get_evs(sns)
    elif not sns and evs:
        get_sns(evs)
    else:
        sns, evs = [Solution.from_generic()], [Evidence.from_generic()]

    return goal, context, sns


'''
- Separate goal, context, soln
- Separate context variable, quantifier
- Detect subgoals/strat

- Incorporate a language model via a suggest button
'''


def main():
    global DBG, logger
    t = datetime.datetime.now()
    tstr = f'{t.day}{t.strftime("%b")}{t.year}_H{t.hour:02d}M{t.minute:02d}S{t.second:02d}M{t.microsecond:07d}'

    logger = setup_logger(tstr)

    try:

        parser = argparse.ArgumentParser(
            description='nlp2Dsl',
            usage='%(prog)s <filename>',
            formatter_class=argparse.ArgumentDefaultsHelpFormatter)
        #parser.add_argument('-f', '--filename', default=None, metavar='file_path.tst')
        parser.add_argument('--debug', action='store_true', help='debug prints')
        args = parser.parse_args()
        DBG = args.debug

        data = input_from_file()

        logger.debug('=' * 20 + ' INPUT ' + '=' * 20)
        logger.debug(data)

        #goal, context, solution = process_input(data)
        goal, context, solution = parse_para(data)

        out = gsn2dsl(goal, context, solution)
    except Exception:
        logger.exception('Runtime error in main().')
        out = gsn2dsl(Goal.from_generic(), Contexts.from_generic(),
                      [Solution.from_generic()])

    print(out)
    logger.debug(out)


if __name__ == '__main__':
    main()
