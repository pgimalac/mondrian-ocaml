NAME = mondrian
SOURCES = geometry.ml \
          bsp/bsp.ml bsp/extrem.ml bsp/classic.ml \
          view.ml main.ml

FOLDER = src/
SOURCES_FP = $(addprefix $(FOLDER), $(SOURCES))

CAMLC = ocamlc
LIBS = graphics.cma
FLAGS = -I src/ -I src/bsp

all: $(NAME)

OBJ = $(SOURCES_FP:.ml=.cmo)
OBJI = $(SOURCES_FP:.ml=.cmi)

p:
	@echo $(OBJ)

%.cmo: %.mli %.ml
	$(CAMLC) $(FLAGS) $(LIBS) -c $? -o $@

%.cmo: %.ml
	$(CAMLC) $(FLAGS) $(LIBS) -c $? -o $@

$(NAME): $(OBJ)
	$(CAMLC) $(FLAGS) $(LIBS) $(OBJ) -o $(NAME)

clean:
	rm -f $(OBJ) $(OBJI)
	rm -f *~ .*~ #*#

fclean: clean
	rm -f $(NAME)

re: fclean all
