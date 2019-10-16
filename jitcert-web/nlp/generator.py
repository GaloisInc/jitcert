def generate(prefix=''):
    import gpt_2_simple as gpt2
    sess = gpt2.start_tf_sess()
    gpt2.load_gpt2(sess, run_name='run1')

    txt = gpt2.generate(sess,
                        length=30,
                        temperature=0.7,
                        prefix=prefix,
                        nsamples=5,
                        batch_size=5)
    #print(txt)
    return txt
