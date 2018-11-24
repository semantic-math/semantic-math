let currentId = ref(0);

let genId = () => {
  let result = currentId^;
  currentId := currentId^ + 1;
  result;
};
