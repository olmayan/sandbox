#ifndef FIELD_H
#define FIELD_H

#include <QIntegerForSize>

class Field
{
public:
    Field(int width, int height);
    Field(Field *field);
    ~Field();

    int width();
    int height();
    int cell(int x, int y);
    void setCell(int x, int y, int value);
    int colors();
    bool throwBalls(Field *throwMap, int nBalls);
    void clear();

    bool hasPath(int x0, int y0, int x, int y, Field *searchResult);

private:
    int *pField;
    int w;
    int h;
    int c;
};

#endif // FIELD_H
