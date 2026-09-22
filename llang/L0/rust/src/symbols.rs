use std::cell::RefCell;
use std::collections::HashMap;

pub type Symbol = i64;

thread_local! {
    static INTERNER: RefCell<(HashMap<String, Symbol>, Vec<String>, Symbol)> =
        RefCell::new((HashMap::new(), Vec::new(), 0));
}

pub fn new_symbol(name: &str) -> Symbol {
    INTERNER.with(|cell| {
        let (map, symbols, next) = &mut *cell.borrow_mut();
        if let Some(sym) = map.get(name) {
            *sym
        } else {
            let sym = *next;
            *next += 1;
            map.insert(name.to_string(), sym);
            symbols.push(name.to_string());
            sym
        }
    })
}

pub fn string_of_symbol(sym: Symbol) -> String {
    INTERNER.with(|cell| {
        let (_, symbols, _) = &*cell.borrow();
        if let Some(s) = symbols.get(sym as usize) {
            s.to_string()
        } else {
            format!("<unknown symbol {}>", sym)
        }
    })
}
