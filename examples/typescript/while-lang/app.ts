import * as While from "./WhileLanguage"

While.begin()
    .while_().true_().do_()
        .if_().num(100).eq().num(150).minus().num(50).then_()
            .while_().true_().do_()
                .skip()
        .else_()
            .skip()
    .andThen()
        .skip()
    .end()
    .accept()
