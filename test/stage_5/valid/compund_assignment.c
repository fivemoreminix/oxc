int main()
{
    int a = 5;
    int b;
    int c;
    b += a;
    c += (b *= 2);
    c /= 2;
    return c == 5;
}
