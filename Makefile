NAME = mondrian
SOURCES = geometry.ml logic.ml sat_solver.ml \
		  bsp/bsp.ml bsp/extrem.ml bsp/classic.ml \
		  interface.ml view/bsp_view.ml view/menu.ml main.ml

FOLDER = src/
SOURCES_FP = $(addprefix $(FOLDER), $(SOURCES))

CAMLC = ocamlc
LIBS = graphics.cma
FLAGS = -I src/ -I src/bsp -I src/view

all: $(NAME)

OBJ = $(SOURCES_FP:.ml=.cmo)
OBJI = $(SOURCES_FP:.ml=.cmi)

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
