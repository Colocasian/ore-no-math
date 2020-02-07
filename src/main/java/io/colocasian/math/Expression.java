package io.colocasian.math;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedList;
import java.util.NoSuchElementException;
import java.util.Queue;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import java.util.Stack;

/**
 * A class implementing methods for evaluating mathematical expressions, with
 * some inbuilt functions, such as variable storage, functions, etc.
 * @author Rishvic Pushpakaran (@Colocasian)
 */
public class Expression {
    private HashMap<String, Double> vars;
    private HashMap<String, String> funs;

    /**
     * Constructor for expression class. Initializes variable register.
     */
    public Expression() {
        this.vars = new HashMap<>();
        this.funs = new HashMap<>();
    }

    /**
     * Method specifying whether a character can be part of a numeric literal.
     * @param c the character to be tested
     * @return boolean Whether the provided character can be part of numeric literal
     */
    private static boolean isNum(char c) {
        return ((c >= '0' && c <= '9') || c == '.');
    }

    /**
     * Method specifying whether a charater can be first character of a variable name. Returns true
     * if yes, else false.
     * @param c the character to be tested
     * @return whether the given character can be first character of a variable name
     */
    private static boolean isVar(char c) {
        return ((c >= 'A' && c <= 'Z') || (c >= 'a' && c <= 'z') || (c == '_'));
    }

    /**
     * Method used to solve a given formula, using the parameters provided, and then return the
     * resulting solution. Throws NoSuchElementException, if provided formula is not defined.
     * Formulae can be overloaded, given they have different number of parameters.
     * @param name the name of the formula
     * @param params the provided parameters
     * @return the answer given by the formula
     * @throws NoSuchElementException if given formula description doesn't exist
     */
    private double solveFormula(String name, double[] params) throws NoSuchElementException {
        String formId = name + "#" + params.length;
        switch (formId) {
            case "sin#1":
                return Math.sin(params[0]);
            case "cos#1":
                return Math.cos(params[0]);
            case "tan#1":
                return Math.tan(params[0]);
            case "asin#1":
                return Math.asin(params[0]);
            case "acos#1":
                return Math.acos(params[0]);
            case "atan#1":
                return Math.atan(params[0]);

            case "exp#1":
                return Math.exp(params[0]);
            case "expm1#1":
                return Math.expm1(params[0]);
            case "log#1":
                return Math.log(params[0]);
            case "log10#1":
                return Math.log10(params[0]);
            case "log1p#1":
                return Math.log1p(params[0]);
            case "log#2":
                return (Math.log(params[0]) / Math.log(params[1]));

            case "sqrt#1":
                return Math.sqrt(params[0]);
            case "cbrt#1":
                return Math.cbrt(params[0]);
            case "hypot#2":
                return Math.hypot(params[0], params[1]);

            case "signum#1":
                return Math.signum(params[0]);

            default:
                if (funs.containsKey(formId))
                    return this.solveCustomFormula(funs.get(formId), params);
                else
                    throw new NoSuchElementException("No such function defined");
        }
    }

    /**
     * Method that solves user-defined formulae.
     * @param formula the formula to be solved
     * @param params the parameters
     * @return the answer
     */
    private double solveCustomFormula(String formula, double[] params) {
        for (int i = 0; i < params.length; i++)
            formula = formula.replace("|" + Integer.toString(i) + "|",
                    "(" + Double.toString(params[i]) + ")");
        return this.evaluate(formula);
    }

    /**
     * Method to be used to define new custom functions. Has built-in checks to only check function
     * name, not the function itself. The formula must contain placeholders for the parameters,
     * where the (zero-indexed) parameters replace "|<param>|", where param is the (zero-indexed)
     * parameter ID.
     * @param fName name of the formula
     * @param paramNums number of parameters
     * @param formula the formula itself, where placeholders for the parameters are writter as "|param#|", where param# is the param number
     * @return whether assignment was successful
     */
    public boolean addFormula(String fName, int paramNums, String formula) {
        if (fName.isEmpty() || (!isVar(fName.charAt(0))))
            return false;
        for (int i = 1; i < fName.length(); i++) {
            char c = fName.charAt(i);
            if ((!isVar(c)) && (c < '0' || c > '9'))
                return false;
        }
        funs.put(fName + "#" + Integer.toString(paramNums), formula);
        return true;
    }

    /**
     * Method used to solve postfix expressions. Assumes that the given postfix expression and the
     * provided numList is correct, so caution is necessary. When null character is present in the
     * postfix expression, it is replaced by the next number in the queue.
     * @param postNote the postfix expression in Queue form
     * @param numList the Queue of all the numbers used in the expression
     * @return the solution of the postfix expression
     */
    private static double solvePostfix(Queue<Character> postNote, Queue<Double> numList) {
        Stack<Double> boya = new Stack<>();
        while (!postNote.isEmpty()) {
            char at = postNote.remove();
            double a, b;
            switch (at) {

                case '\0':
                    boya.push(numList.remove());
                    break;

                case '^':
                    b = boya.pop();
                    a = boya.pop();
                    boya.push(Math.pow(a, b));
                    break;

                case '@':
                    a = boya.pop();
                    boya.push(-a);
                    break;

                case '*':
                    b = boya.pop();
                    a = boya.pop();
                    boya.push(a * b);
                    break;

                case '/':
                    b = boya.pop();
                    a = boya.pop();
                    boya.push(a / b);
                    break;

                case '+':
                    b = boya.pop();
                    a = boya.pop();
                    boya.push(a + b);
                    break;

                case '-':
                    b = boya.pop();
                    a = boya.pop();
                    boya.push(a - b);
                    break;
            }
        }

        return boya.peek();
    }

    /**
     * Public method used to evaluate any infix expression in string form. Implemented many checks
     * within the method to prevent parsing of incorrect expressions.
     * @param infix the infix expression
     * @return the resulting answer from the infix expression
     * @throws ArithmeticException if the provided infix expression has incorrect format
     * @throws NoSuchElementException if any included variable is not defined
     */
    public double evaluate(String infix) throws ArithmeticException, NoSuchElementException {
        if (infix.trim().isEmpty())
            throw new ArithmeticException("empty string passed");
        Queue<Double> nums = new LinkedList<>();
        Stack<Character> chars = new Stack<>();
        chars.push('(');
        
        Queue<Character> postfix = new LinkedList<>();

        boolean nxtNum = true;
        boolean nxt1op = true;
        boolean nxt2op = false;
        boolean nxtBro = true;
        boolean nxtBrc = false;
        int lvl = 0;

        for (int i = 0; i < infix.length(); i++) {
            char at = infix.charAt(i);

            if (at == '(' || at == '[' || at == '{') {
                if (!nxtBro)
                    throw new ArithmeticException("not expecting opening braces");
                chars.push(at);
                lvl++;

                nxtNum = true;
                nxt1op = true;
                nxt2op = false;
                nxtBro = true;
                nxtBrc = false;
            }
            else if (at == ')' || at == ']' || at == '}') {
                if (!nxtBrc)
                    throw new ArithmeticException("not expecting closing braces");

                while (chars.peek() != '(' && chars.peek() != '[' &&
                        chars.peek() != '{')
                    postfix.add(chars.pop());

                if ((at == ')' && chars.peek() != '(') || (at == ']' && chars.peek() != '[') || 
                        (at == '}' && chars.peek() != '{'))
                    throw new ArithmeticException("incorrect bracket matching");

                chars.pop();
                lvl--;

                nxtNum = false;
                nxt1op = false;
                nxt2op = true;
                nxtBro = false;
                nxtBrc = (lvl > 0);
            }
            else if (isNum(at)) {
                if (!nxtNum)
                    throw new ArithmeticException("not expecting number");
                int j = i+1;
                while (j < infix.length() && isNum(infix.charAt(j)))
                    j++;
                if (j < infix.length() && (infix.charAt(j) == 'e' || infix.charAt(j) == 'E')) {
                    j++;
                    if (j < infix.length() && (infix.charAt(j) == '+' || infix.charAt(j) == '-'))
                        j++;
                    while (j < infix.length() && isNum(infix.charAt(j)))
                        j++;
                }

                postfix.add('\0');
                nums.add(Double.parseDouble(infix.substring(i, j)));
                i = j-1;

                nxtNum = false;
                nxt1op = false;
                nxt2op = true;
                nxtBro = false;
                nxtBrc = (lvl > 0);
            }
            else if (isVar(at)) {
                if (!nxtNum)
                    throw new ArithmeticException("not expecting variable name");
                int j = i+1;
                while (j < infix.length() && (isVar(infix.charAt(j)) ||
                            (infix.charAt(j) >= '0' && infix.charAt(j) <= '9')))
                    j++;

                String varName = infix.substring(i, j);
                if (j != infix.length() && infix.charAt(j) == '(') {
                    ArrayList<Integer> stops = new ArrayList<>();
                    stops.add(j);
                    int k = j+1;
                    int funcLvl = 1;
                    while (k != infix.length() && funcLvl != 0) {
                        switch (infix.charAt(k)) {
                            case '(':
                                funcLvl++;
                                break;
                            case ')':
                                funcLvl--;
                                break;
                            case ',':
                                if (funcLvl == 1)
                                    stops.add(k);
                                break;
                        }
                        k++;
                    }
                    if (funcLvl != 0)
                        throw new ArithmeticException("Unclosed braces");

                    stops.add(k-1);

                    // String[] paramStr = infix.substring(j+1, k-1).split(",");
                    int paramNum = stops.size() - 1;
                    double[] paramDbl = new double[paramNum];

                    for (int l = 0; l < paramNum; l++)
                        paramDbl[l] = this.evaluate(infix.substring(stops.get(l)+1, stops.get(l+1)));

                    postfix.add('\0');
                    nums.add(this.solveFormula(varName, paramDbl));
                    i = k-1;
                }
                else {
                    if (!vars.containsKey(varName))
                        throw new NoSuchElementException("no such variable exists");
                    postfix.add('\0');
                    nums.add(vars.get(varName));
                    i = j-1;
                }

                nxtNum = false;
                nxt1op = false;
                nxt2op = true;
                nxtBro = false;
                nxtBrc = (lvl > 0);
            }
            else if (at == '+' || at == '-' || at == '*' || at == '/' || at == '^') {
                if (at == '^') {
                    if (!nxt2op)
                        throw new ArithmeticException("not expecting bin operator");
                    chars.push(at);

                    nxtNum = true;
                    nxt1op = false;
                    nxt2op = false;
                    nxtBro = true;
                    nxtBrc = false;
                }
                if (at == '*' || at == '/') {
                    if (!nxt2op)
                        throw new ArithmeticException("not expecting bin operator");
                    while (chars.peek() == '^' || chars.peek() == '@' || chars.peek() == '*' ||
                            chars.peek() == '/')
                        postfix.add(chars.pop());

                    chars.push(at);

                    nxtNum = true;
                    nxt1op = false;
                    nxt2op = false;
                    nxtBro = true;
                    nxtBrc = false;
                }
                else if (at == '+' || at == '-') {
                    if (nxt2op) {
                        while (chars.peek() == '^' || chars.peek() == '@' || chars.peek() == '*' ||
                                chars.peek() == '/' || chars.peek() == '+' || chars.peek() == '-')
                            postfix.add(chars.pop());

                        chars.push(at);

                        nxtNum = true;
                        nxt1op = false;
                        nxt2op = false;
                        nxtBro = true;
                        nxtBrc = false;
                    }
                    else {
                        if (!nxt1op)
                            throw new ArithmeticException("not expecting operator");
                        if (at == '-') {
                            while (chars.peek() == '^')
                                postfix.add(chars.pop());

                            chars.push('@');
                        }

                        nxtNum = true;
                        nxt1op = false;
                        nxt2op = false;
                        nxtBro = true;
                        nxtBrc= false;
                    }
                }
            }
            else if (at != ' ')
                throw new ArithmeticException("unexpected character");
        }
        if (lvl != 0)
            throw new ArithmeticException("unbalanced braces");

        while (chars.peek() != '(')
            postfix.add(chars.pop());

        return solvePostfix(postfix, nums);
    }

    /**
     * Public method used to define a variable with a corresponding value. Returns true if given
     * variable name is valid and assignment is done. If given name is invalid, doesn't assign the
     * value and returns false.
     * @param name the variable name to be assigned
     * @param num the variable value to be assigned
     * @return whether assignment was successful or not
     */
    public boolean setVariable(String name, double num) {
        if (name.isEmpty() || (!isVar(name.charAt(0))))
            return false;
        for (int i = 1; i < name.length(); i++) {
            if ((!isVar(name.charAt(i))) && (name.charAt(i) < '0' || name.charAt(i) > '9'))
                return false;
        }
        vars.put(name, num);
        return true;
    }

}
