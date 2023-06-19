import {test, registerTest, TestResult} from '../compiled/examples/nestedData.js';

export async function nestedDataCall(): Promise<TestResult> {
    let nested = {
        one: {
            val: "hello"
        }
    }
    registerTest({
        test1: () => {
            return nested;
        },
        test2: (arg1: { val: string; }, arg2: string) => {
            let res = {
                one: {
                    val: (arg1.val + arg2)
                }
            }
            return res
        }
    });

    return await test();
}
