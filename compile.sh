erlc *.erl;
mkdir ebin;
mv *.beam ebin/;
cp .hosts.erlang ebin/;
make -C driver/;
cd ebin/;
erl
