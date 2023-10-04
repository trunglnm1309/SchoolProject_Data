import * as types from "./../constants/ActionType.js";

export const showLogin = () => {
  return {
    type: types.SHOW_MODAL_LOGIN,
  };
};

export const showRegister = () => {
  return {
    type: types.SHOW_MODAL_REGISTER,
  };
};

export const closeLogin = () => {
  return {
    type: types.CLOSE_MODAL_LOGIN,
  };
};
export const deleteAllCart = () => {
  return {
    type: types.DELETE_ALL_CART_ITEM,
  };
};
export const showForgotPassword = () => {
  return {
    type: types.SHOW_FORGET_PASSWORD,
  };
};
export const addToCart = (product) => {
  return {
    type: types.ADD_TO_CART,
    product,
  };
};

export const changeQuantityCart = (number, product) => {
  return {
    type: types.CHANGE_QUANTITY_CART,
    number,
    product,
  };
};

export const deleteCartItem = (product) => {
  return {
    type: types.DELETE_CART_ITEM,
    product,
  };
};
