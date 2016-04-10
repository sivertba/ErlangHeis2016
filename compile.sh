erlc *.erl;
mkdir ebin;
mv *.beam ebin/;
make -C driver/;
cd ebin/;
erl