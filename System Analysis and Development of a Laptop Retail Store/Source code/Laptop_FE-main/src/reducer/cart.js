import * as types from '../constants/ActionType';
var data = JSON.parse(localStorage.getItem('Product'))
const initialState = data ? data : [];

const reducer = (state = initialState, action) => {
    const { product, number } = action;
    switch (action.type) {
        case types.ADD_TO_CART:
            if (!product.soluong && product.soluong != 1) {
                product.soluong = 1;
            }
            for (let i = 0; i < state.length; i++) {
                if (product.id == state[i].id) {
                    product.soluong += 1;
                    state.splice(state.length - 1, 1);
                    break;
                }
            }
            state.push(product);
            localStorage.setItem('Product', JSON.stringify([...state]));
            return [...state];
        case types.CHANGE_QUANTITY_CART:
            for (let i = 0; i < state.length; i++) {
                if (product.id == state[i].id) {
                    if(product.soluong+number>0){
                        product.soluong += number;
                        state.splice(i, 1, product);
                    }
                    else
                        state.splice(i, 1);
                    break;
                }
            }
            localStorage.setItem('Product', JSON.stringify([...state]));
            return [...state];
        case types.DELETE_CART_ITEM:
            for (let i = 0; i < state.length; i++) {
                if (product.id == state[i].id) {
                    state.splice(i, 1);
                }
            }
            localStorage.setItem('Product', JSON.stringify([...state]));
            return [...state];
        case types.DELETE_ALL_CART_ITEM:
            localStorage.removeItem('Product');
            return [];
        default:
            return state;
    }
}
export default reducer;