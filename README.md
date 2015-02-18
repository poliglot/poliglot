# Poliglot
Poliglot is a Scala library for parsing common language resources such as corpora and tagsets. It was created in order to facilitate working with bilingual corpora.

Currently, it has language support for:

- **German:** [RFTagger](http://www.cis.uni-muenchen.de/~schmid/tools/RFTagger/) tagset
- **Polish:** [NKJP](http://nkjp.pl/) tagset, TCP bindings for [concraft-pl](https://github.com/kawu/concraft-pl)

The language-specific tagsets are translated to a generic class-based hierarchy.

## Corpus creation and analysis
Poliglot also ships the following tools for creating bilingual corpora and analysing annotations:

### ``AnalyseAdpositions``
Analyse the semantics of adpositions.

### ``AnalyseCorpus``
Prints statistics on the bilingual corpus.

### ``AnnotateSentences``
Annotates selected sentences morphosyntactically. To achieve that tokenisation is consistent, the German sentences are tokenised by concraft-pl. The annotated alignments are written to ``alignments-import.xml``. This dump can then be imported using the [corpus editor](https://github.com/poliglot/poliglot-ui) into the existing database (``alignments.xml``).

### ``DeriveAlignments``
Train a model for alignment entities.

### ``ExtractLemmas``
Reads the alignment corpus and extracts potential lemmas that are missing in the lemma corpus.

### ``SelectSentences``
Manually select alignments from the provided ``.tmx`` file. It can be obtained from [here](http://opus.lingfil.uu.se/). The dump is expected to provide German-Polish alignments for now. The purpose is to select viable sentences for each of the German adpositions defined in ``German.Adpositions``. Sentences are tagged with a flag indicating their fitness. It can be executed several times; all prior tagged sentences will be skipped over in this case.

## See also
[This document](https://github.com/tindzk/thesis) explains the underlying concepts and provides an analysis of the [German-Polish corpus](https://github.com/poliglot/parallel-de-pl) with regards to the annotation of adpositions.

## License
Poliglot is licensed under the terms of the Apache v2.0 license.

## Authors
- Tim Nieradzik
