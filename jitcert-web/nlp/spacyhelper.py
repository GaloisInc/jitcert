import spacy
from spacy.symbols import nsubj, VERB

nlp = spacy.load('en')


def parse_sent(text):
    nlp = spacy.load('en')
    doc = nlp(text)
    s = list(doc.sents)
    assert (len(s) == 1)
    return s[0]


def sentences(text):
    doc = nlp(text)
    s = list(doc.sents)
    return s


def tokenize(sent):
    doc = nlp(sent)
    return (tok.text for tok in doc)


def constituents(txt):
    import benepar.spacy_plugin as b
    nlp = spacy.load('en')
    nlp.add_pipe(b.BeneparComponent('benepar_en2'))
    doc = nlp(txt)
    for sent in doc.sents:
        if __debug__:
            print(sent._.parse_string)
            print(sent._.labels)
    if sent._.labels[0] == 'S' and len(sent._.labels) == 1:
        l = list(sent._.children)
        np = [i for i in l if 'NP' in i._.labels]
        vp = [i for i in l if 'VP' in i._.labels]
        if len(np) != 1:
            raise NotImplementedError
        if len(vp) != 1:
            raise NotImplementedError
        print(f'Noun Phrase: {np}')
        print(f'Verb Phrase: {vp}')
    else:
        raise NotImplementedError
    #label_vocab = b.get_constituent(sent)[0].label_vocab
    #for l, token in zip((label_vocab[t] for t in b.get_constituent(sent)[0].labels), b.get_subconstituents(sent)):
    #    print(f'{token}: {l}')

    #ex: '(S (NP (DT The) (JJ cryptographic) (NN boundary)) (VP (VBZ is) (ADJP (RB well) (VBN defined))) (. .))'
