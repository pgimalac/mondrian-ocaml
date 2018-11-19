NAME = mondrian
SOURCES = bsp.ml view.ml
MAIN = main.ml

CAMLC = ocamlc

LIBS = graphics.cma

FLAGS =

all: $(NAME)

OBJS = $(SOURCES:.ml=.cmo)

$(NAME): $(OBJS)
	$(CAMLC) $(FLAGS) $(LIBS) $(OBJS) $(MAIN) -o $(NAME)

%.cmo: %.ml
	$(CAMLC) $(FLAGS) $(LIBS) -c $?i $? -o $@

clean:
	rm -f *.cm[iox] *~ .*~ #*#
	rm -f $(NAME)

re: clean all
